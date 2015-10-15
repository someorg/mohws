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

module Network.MoHWS.HTTP.Response where

import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.Stream as Stream
import Network.MoHWS.HTTP.Header (HasHeaders, )
import Network.MoHWS.ParserUtility (crLf, )
import Network.MoHWS.Utility (formatTimeSensibly, hPutStrCrLf, )

import Control.Monad.Trans.State (state, evalState, get, )
import Data.Tuple.HT (swap, )

import qualified Network.HTTP.Base as HTTP
import qualified Network.HTTP.Headers
   -- make getHeaders visible for instance declaration
import Network.URI (URI, )

import qualified Data.Map as Map

import qualified Control.Exception as Exception
import qualified System.IO as IO
import System.Time (getClockTime, toUTCTime, )
import qualified Text.Html as Html
import Text.Html (Html, renderHtml, toHtml, noHtml, (+++), (<<), )


-----------------------------------------------------------------------------
-- Responses

data Body body =
   Body {
      -- e.g. filename of content
      source  :: String,
      size    :: Maybe Integer,
      close   :: IO (),
      content :: body
   }

data T body =
   Cons {
      code         :: Int,
      description  :: String,
      headers      :: Header.Group,
      coding       :: [Header.TransferCoding],
         {- either empty or terminated with ChunkedTransferEncoding
            (RFC2616, sec 3.6) -}
      doSendBody   :: Bool,
         {- actually send the body?
            (False for HEAD requests) -}
      body         :: Body body
   }

instance Functor Body where
   fmap f bdy =
      Body {
         source  =     source  bdy,
         size    =     size    bdy,
         close   =     close   bdy,
         content = f $ content bdy
      }

instance Functor T where
   fmap f resp =
      Cons {
         code        =          code        resp,
         description =          description resp,
         headers     =          headers     resp,
         coding      =          coding      resp,
         doSendBody  =          doSendBody  resp,
         body        = fmap f $ body        resp
      }

decomposeCode :: Int -> HTTP.ResponseCode
decomposeCode =
   let getDigit = state $ swap . flip divMod 10
   in  evalState $
          do c <- getDigit
             b <- getDigit
             a <- get
             return (a,b,c)

toHTTPbis :: T body -> HTTP.Response body
toHTTPbis resp =
   HTTP.Response {
      HTTP.rspCode    = decomposeCode (code resp),
      HTTP.rspReason  = description resp,
      HTTP.rspHeaders = Header.ungroup $ headers resp,
      HTTP.rspBody    = content $ body resp
   }

fromHTTPbis :: HTTP.Response body -> T body
fromHTTPbis resp =
   Cons {
      code         =
         let (a,b,c) = HTTP.rspCode resp
         in  (a*10+b)*10+c,
      description  = HTTP.rspReason resp,
      headers      = Header.group $ HTTP.rspHeaders resp,
      coding       = [],
      doSendBody   = True,
      body         =
         Body {
            source = "HTTPbis response",
            size = Nothing,
            close = return (),
            content = HTTP.rspBody resp
         }
   }


instance Show (T body) where
   showsPrec _ r =
      showString (showStatusLine r) . showString crLf .
      shows (headers r)

instance HasHeaders (T body) where
    getHeaders = Header.ungroup . headers
    setHeaders resp hs = resp { headers = Header.group hs}

showStatusLine :: T body -> String
showStatusLine (Cons s desc _ _ _ _) = show s ++ " " ++ desc


hasBody :: (Stream.C body) => Body body -> Bool
hasBody = not . Stream.isEmpty . content

getFileName :: Body body -> String
getFileName = source

sendBody :: (Stream.C body) => IO.Handle -> Body body -> IO ()
sendBody h b =
   Exception.finally
     (do Stream.write h $ content b
         IO.hFlush h)
     {-
     It is only safe to close the source
     after all lazily read data is written.
     -}
     (close b)

sendBodyChunked :: (Stream.C body) =>
   Int -> IO.Handle -> Body body -> IO ()
sendBodyChunked chunkSize h b =
   Exception.finally
     (do Stream.writeChunked chunkSize h $ content b
         hPutStrCrLf h "0"
         hPutStrCrLf h ""
         IO.hFlush h)
     {-
     It is only safe to close the source
     after all lazily read data is written.
     -}
     (close b)


-- only allowed in chunked coding
bodyFromString :: (Stream.C body) => body -> Body body
bodyFromString str =
   Body {
      source = "<generated>",
      size = Nothing,
      close = return (),
      content = str
   }

bodyWithSizeFromString :: (Stream.C body) => body -> Body body
bodyWithSizeFromString str =
   Body {
      source = "<generated>",
      size = Just $ Stream.length str,
      close = return (),
      content = str
   }

statusLine :: Int -> String -> String
statusLine cde desc = httpVersion ++ ' ': show cde ++ ' ': desc

httpVersion :: String
httpVersion = "HTTP/1.1"


-----------------------------------------------------------------------------
-- Response Header.Group

dateHeader :: IO Header.T
dateHeader = do
   -- Dates in HTTP/1.1 have to be GMT, which is equivalent to UTC
  fmap
     (Header.make Header.HdrDate .
      formatTimeSensibly .
      toUTCTime)
     getClockTime

serverHeader :: Header.T
serverHeader =
   Header.make Header.HdrServer $
   Config.serverSoftware ++ '/':Config.serverVersion


-----------------------------------------------------------------------------
-- Response codes

makeCont :: (Stream.C body) => Config.T ext -> T body
makeCont                         = makeError 100
makeSwitchingProtocols :: (Stream.C body) => Config.T ext -> T body
makeSwitchingProtocols           = makeError 101
makeOk :: Config.T ext -> Bool -> Header.Group -> Body body -> T body
makeOk                           = makeWithBody 200
makeCreated :: (Stream.C body) => Config.T ext -> T body
makeCreated                      = makeError 201
makeAccepted :: (Stream.C body) => Config.T ext -> T body
makeAccepted                     = makeError 202
makeNonAuthoritiveInformation :: (Stream.C body) => Config.T ext -> T body
makeNonAuthoritiveInformation    = makeError 203
makeNoContent :: (Stream.C body) => Config.T ext -> T body
makeNoContent                    = makeError 204
makeResetContent :: (Stream.C body) => Config.T ext -> T body
makeResetContent                 = makeError 205
makePartialContent :: (Stream.C body) => Config.T ext -> T body
makePartialContent               = makeError 206
makeMultipleChoices :: (Stream.C body) => Config.T ext -> T body
makeMultipleChoices              = makeError 300
makeMovedPermanently :: Config.T ext -> Header.Group -> Body body -> URI -> T body
makeMovedPermanently conf hdrs bdy uri =
   makeWithBody 301 conf True
      (Header.modifyMany (Header.makeLocation uri :) hdrs) bdy
makeFound :: (Stream.C body) => Config.T ext -> T body
makeFound                        = makeError 302
makeSeeOther :: (Stream.C body) => Config.T ext -> T body
makeSeeOther                     = makeError 303
makeNotModified :: (Stream.C body) => Config.T ext -> T body
makeNotModified                  = makeError 304
makeUseProxy :: (Stream.C body) => Config.T ext -> T body
makeUseProxy                     = makeError 305
makeTemporaryRedirect :: (Stream.C body) => Config.T ext -> T body
makeTemporaryRedirect            = makeError 307
makeBadRequest :: (Stream.C body) => Config.T ext -> T body
makeBadRequest                   = makeError 400
makeUnauthorized :: (Stream.C body) => Config.T ext -> T body
makeUnauthorized                 = makeError 401
makePaymentRequired :: (Stream.C body) => Config.T ext -> T body
makePaymentRequired              = makeError 402
makeForbidden :: (Stream.C body) => Config.T ext -> T body
makeForbidden                    = makeError 403
makeNotFound :: (Stream.C body) => Config.T ext -> T body
makeNotFound                     = makeError 404
makeMethodNotAllowed :: (Stream.C body) => Config.T ext -> T body
makeMethodNotAllowed             = makeError 405
makeNotAcceptable :: (Stream.C body) => Config.T ext -> T body
makeNotAcceptable                = makeError 406
makeProxyAuthenticationRequired :: (Stream.C body) => Config.T ext -> T body
makeProxyAuthenticationRequired  = makeError 407
makeRequestTimeOut :: (Stream.C body) => Config.T ext -> T body
makeRequestTimeOut               = makeError 408
makeConflict :: (Stream.C body) => Config.T ext -> T body
makeConflict                     = makeError 409
makeGone :: (Stream.C body) => Config.T ext -> T body
makeGone                         = makeError 410
makeLengthRequired :: (Stream.C body) => Config.T ext -> T body
makeLengthRequired               = makeError 411
makePreconditionFailed :: (Stream.C body) => Config.T ext -> T body
makePreconditionFailed           = makeError 412
makeRequestEntityTooLarge :: (Stream.C body) => Config.T ext -> T body
makeRequestEntityTooLarge        = makeError 413
makeRequestURITooLarge :: (Stream.C body) => Config.T ext -> T body
makeRequestURITooLarge           = makeError 414
makeUnsupportedMediaType :: (Stream.C body) => Config.T ext -> T body
makeUnsupportedMediaType         = makeError 415
makeRequestedRangeNotSatisfiable :: (Stream.C body) => Config.T ext -> T body
makeRequestedRangeNotSatisfiable = makeError 416
makeExpectationFailed :: (Stream.C body) => Config.T ext -> T body
makeExpectationFailed            = makeError 417
makeInternalServerError :: (Stream.C body) => Config.T ext -> T body
makeInternalServerError          = makeError 500
makeNotImplemented :: (Stream.C body) => Config.T ext -> T body
makeNotImplemented               = makeError 501
makeBadGateway :: (Stream.C body) => Config.T ext -> T body
makeBadGateway                   = makeError 502
makeServiceUnavailable :: (Stream.C body) => Config.T ext -> T body
makeServiceUnavailable           = makeError 503
makeGatewayTimeOut :: (Stream.C body) => Config.T ext -> T body
makeGatewayTimeOut               = makeError 504
makeVersionNotSupported :: (Stream.C body) => Config.T ext -> T body
makeVersionNotSupported          = makeError 505

descriptionDictionary :: Map.Map Int String
descriptionDictionary =
   Map.fromList $

   (100, "Continue") :
   (101, "Switching Protocols") :

   (200, "OK") :
   (201, "Created") :
   (202, "Accepted") :
   (203, "Non-Authoritative Information") :
   (204, "No Content") :
   (205, "Reset Content") :
   (206, "Partial Content") :

   (300, "Multiple Choices") :
   (301, "Moved Permanently") :
   (302, "Found") :
   (303, "See Other") :
   (304, "Not Modified") :
   (305, "Use Proxy") :
   (307, "Temporary Redirect") :

   (400, "Bad Request") :
   (401, "Unauthorized") :
   (402, "Payment Required") :
   (403, "Forbidden") :
   (404, "Not Found") :
   (405, "Method Not Allowed") :
   (406, "Not Acceptable") :
   (407, "Proxy Authentication Required") :
   (408, "Request Time-out") :
   (409, "Conflict") :
   (410, "Gone") :
   (411, "Length Required") :
   (412, "Precondition Failed") :
   (413, "Request Entity Too Large") :
   (414, "Request-URI Too Large") :
   (415, "Unsupported Media Type") :
   (416, "Requested range not satisfiable") :
   (417, "Expectation Failed") :

   (500, "Internal Server Error") :
   (501, "Not Implemented") :
   (502, "Bad Gateway") :
   (503, "Service Unavailable") :
   (504, "Gateway Time-out") :
   (505, "HTTP Version not supported") :
   []

descriptionFromCode :: Int -> String
descriptionFromCode c =
   Map.findWithDefault "Unknown response" c descriptionDictionary

makeError :: (Stream.C body) =>
   Int -> Config.T ext -> T body
makeError cde conf =
   makeWithBody cde conf True
      (Header.group [Header.makeContentType "text/html"])
      (generateErrorPage cde conf)

makeWithBody :: Int -> Config.T ext -> Bool -> Header.Group -> Body body -> T body
makeWithBody cde _conf doSend hdrs bdy =
   Cons cde (descriptionFromCode cde) hdrs [] doSend bdy

-----------------------------------------------------------------------------
-- Error pages

-- We generate some html for the client to display on an error.

generateErrorPage :: (Stream.C body) =>
   Int -> Config.T ext -> Body body
generateErrorPage cde conf =
   bodyWithSizeFromString $ Stream.fromString (Config.chunkSize conf) $
   renderHtml $ genErrorHtml cde conf

genErrorHtml :: Int -> Config.T ext -> Html
genErrorHtml cde conf =
   let statusLn =
          show cde +++ ' ' +++ descriptionFromCode cde
   in  Html.header << Html.thetitle << statusLn
         +++ Html.body <<
              (Html.h1 << statusLn
               +++ Html.hr
               +++ Config.serverSoftware +++ '/' +++ Config.serverVersion
               -- ToDo: use real hostname if we don't have a serverName
               +++ case Config.serverName conf of
                     "" -> noHtml
                     me -> " on " +++ me +++ Html.br
               +++ case Config.serverAdmin conf of
                     "" -> noHtml
                     her -> "Server Admin: " +++
                            Html.hotlink ("mailto:"++her) [toHtml her]
              )
