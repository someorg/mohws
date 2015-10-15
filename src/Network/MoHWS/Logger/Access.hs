-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
-- Copyright 2009, Henning Thielemann.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module Network.MoHWS.Logger.Access (
    Handle,
    Request(..),
    start,
    stop,
    mkRequest,
    log,
  ) where

import qualified Network.MoHWS.Logger as Logger
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Server.Request as ServerRequest
import Network.MoHWS.Utility (formatTimeSensibly, )

import Network.BSD (HostEntry, hostName, )
import qualified Network.Socket as Socket
import System.Time (ClockTime, toUTCTime, getClockTime, TimeDiff, timeDiffToString, )
import Control.Monad (liftM, liftM2, )

import Prelude hiding (log, )


type Handle = Logger.Handle Request

{-
FIXME:
Instead of using body type ()
we should have data structures for the Response and Request headers
without the body,
like ResponseData and RequestData that are internally used in Network.HTTP.
-}
data Request = Request
   {
      request     :: ServerRequest.T (),
      response    :: Response.T (),
      serverHost  :: HostEntry,
      time        :: ClockTime,
      delay       :: TimeDiff
   }


start :: String -> FilePath -> IO Handle
start format file = Logger.start (mkLine format) file

{-
Instead of the class we could just use IO monad,
but I like to make explicit,
what are the functions that force us to do IO.
-}
class Monad m => Help m where
   inet_ntoa :: Socket.HostAddress -> m String

instance Help IO where
   inet_ntoa = Socket.inet_ntoa

infixr 5 +^+, ^:

(+^+) :: Monad m => m [a] -> m [a] -> m [a]
(+^+) = liftM2 (++)

(^:) :: Monad m => a -> m [a] -> m [a]
(^:) x = liftM (x:)

mkLine :: Help m => String -> Request -> m String
mkLine "" _ = return ""
mkLine ('%':'{':rest) r =
    case span (/= '}') rest of
      (str, '}':c:rest1) -> expand (Just str) c r +^+ mkLine rest1 r
      _                  -> '%' ^: '{' ^: mkLine rest r
mkLine ('%':c:rest) r = expand Nothing c r +^+ mkLine rest r
mkLine (c:rest) r = c ^: mkLine rest r

expand :: Help m => Maybe String -> Char -> Request -> m String
expand arg c info =
   let resp = response info
       sreq = request info
       req  = ServerRequest.clientRequest sreq
       -- host = clientName (log_request info)
       header _ Nothing  = ""
       header x (Just n) = unwords (Header.lookupMany (Header.makeName n) x)
       addr = inet_ntoa (ServerRequest.clientAddress sreq)
   in  case c of
         'b' -> return $ maybe "unknown" show $ Response.size (Response.body resp)
         'f' -> return $ ServerRequest.serverFilename sreq

         -- %h is the hostname if hostnameLookups is on, otherwise the
         -- IP address.
         'h' -> maybe addr (return . hostName) (ServerRequest.clientName sreq)
         'a' -> addr
         'l' -> return "-" -- FIXME: does anyone use identd these days?
         'r' -> return $ show req
         -- ToDo: 'p' -> canonical port number of server
         's' -> return $ show (Response.code resp)
         't' -> return $ formatTimeSensibly (toUTCTime (time info))
         'T' -> return $ timeDiffToString (delay info)
         'v' -> return $ hostName (serverHost info)
         'u' -> return "-" -- FIXME: implement HTTP auth

         'i' -> return $ header req arg
         'o' -> return $ header resp arg

         -- ToDo: other stuff
         _ -> return ['%',c]

stop :: Handle -> IO ()
stop l = Logger.stop l

mkRequest :: ServerRequest.T body -> Response.T body -> HostEntry -> TimeDiff -> IO Request
mkRequest req resp host delay0 =
    do time0 <- getClockTime
       return $
          Request {
             request     = fmap (const ()) req,
             response    = fmap (const ()) resp,
             serverHost  = host,
             time        = time0,
             delay       = delay0
          }

log :: Handle -> Request -> IO ()
log l r = Logger.log l r
