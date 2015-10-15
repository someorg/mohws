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

module Network.MoHWS.HTTP.Request (
      T(Cons), command, uri, httpVersion, headers, body,
      toHTTPbis, fromHTTPbis,
      Command, HTTP.RequestMethod(..),
      Connection(..),
      Expect(..),
      pHeaders,
      getHost,
      getConnection,
   ) where

import Text.ParserCombinators.Parsec (Parser, skipMany1, many, noneOf, )
import Network.MoHWS.ParserUtility (pCRLF, pSP, pToken, parseList, )

import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Version as HTTPVersion
import Network.MoHWS.HTTP.Header (HasHeaders, )
import Network.MoHWS.Utility (readM, )

import qualified Network.HTTP.Base as HTTP
import qualified Network.HTTP.Headers
   -- make getHeaders visible for instance declaration

import Network.Socket (HostName, )
import Network.URI (URI, nullURI, uriPath, uriQuery, )

import qualified Data.Map as Map
import Data.Monoid (Monoid, mempty, )
import Data.Char (toLower, )


-----------------------------------------------------------------------------
-- Requests

-- Request-Line   = Method SP Request-URI SP HTTP-Version CRLF

type Command = HTTP.RequestMethod

data T body =
   Cons {
      command     :: Command,
      uri         :: URI,
      httpVersion :: HTTPVersion.T,
      headers     :: Header.Group,
      body        :: body
   }


toHTTPbis :: T body -> HTTP.Request body
toHTTPbis req =
   HTTP.Request {
      HTTP.rqURI     = uri req,
      HTTP.rqMethod  = command req,
      HTTP.rqHeaders = Header.ungroup $ headers req,
      HTTP.rqBody    = body req
   }

fromHTTPbis :: HTTP.Request body -> T body
fromHTTPbis req =
   Cons {
      command     = HTTP.rqMethod req,
      uri         = HTTP.rqURI req,
      httpVersion = HTTPVersion.http1_1,
      headers     = Header.group $ HTTP.rqHeaders req,
      body        = HTTP.rqBody req
   }


instance Show (T body) where
   showsPrec _ Cons{command = cmd, uri = loc, httpVersion = ver} =
      shows cmd . (' ':) . shows loc . (' ':) . shows ver

instance HasHeaders (T body) where
   getHeaders = Header.ungroup . headers
   setHeaders req hs = req { headers = Header.group hs}

instance Functor T where
   fmap f req =
      Cons {
         command     = command     req,
         uri         = uri         req,
         httpVersion = httpVersion req,
         headers     = headers     req,
         body        = f $ body req
      }



-- Request parsing

-- Parse the request line and the headers, but not the body.
pHeaders :: Monoid body => Parser (T body)
pHeaders =
   do (cmd,loc,ver) <- pCommandLine
      hdrs <- Header.pGroup
      _ <- pCRLF
      return $ Cons cmd loc ver hdrs mempty

pCommandLine :: Parser (Command, URI, HTTPVersion.T)
pCommandLine =
   do cmd <- pCommand
      skipMany1 pSP
      loc <- pURI
      skipMany1 pSP
      ver <- HTTPVersion.pInRequest
      _ <- pCRLF
      return (cmd,loc,ver)

commandDictionary :: Map.Map String Command
commandDictionary =
   Map.fromList $
   ("HEAD",    HTTP.HEAD)    :
   ("PUT",     HTTP.PUT)     :
   ("GET",     HTTP.GET)     :
   ("POST",    HTTP.POST)    :
   ("DELETE",  HTTP.DELETE)  :
   ("OPTIONS", HTTP.OPTIONS) :
   ("TRACE",   HTTP.TRACE)   :
--   ("CONNECT", HTTP.CONNECT) :
   []

pCommand :: Parser Command
pCommand =
   fmap (\tok -> Map.findWithDefault (HTTP.Custom tok) tok commandDictionary) $
   pToken

pURI :: Parser URI
pURI =
   do u <- many (noneOf [' '])
      -- FIXME: this does not handle authority Request-URIs
      -- maybe (fail "Bad Request-URI") return $ parseURIReference u
      return $ laxParseURIReference u

-- also accepts characters [ ] " in queries, which is sometimes quite handy
laxParseURIReference :: String -> URI
laxParseURIReference u =
   let (p,q) = break ('?'==) u
   in  nullURI{uriPath=p, uriQuery=q}

-----------------------------------------------------------------------------
-- Getting specific request headers


data Connection =
     ConnectionClose
   | ConnectionKeepAlive -- non-std?  Netscape generates it.
   | ConnectionOther String
   deriving (Eq, Show)

parseConnection :: String -> [Connection]
parseConnection =
   let fn "close"      = ConnectionClose
       fn "keep-alive" = ConnectionKeepAlive
       fn other        = ConnectionOther other
   in  map (fn . map toLower) . parseList

getConnection :: HasHeaders a => a -> [Connection]
getConnection =
   concatMap parseConnection . Header.lookupMany Header.HdrConnection

data Expect = ExpectContinue
  deriving Show

-- parseExpect :: String -> Maybe Expect
-- parseExpect s =
--   case parseList s of
--      ["100-continue"] -> Just ExpectContinue
--      _                -> Nothing


getHost :: HasHeaders a => a -> Maybe (HostName, Maybe Int)
getHost x = Header.lookup Header.HdrHost x >>= parseHost

parseHost :: String -> Maybe (HostName, Maybe Int)
parseHost s =
   let (host,prt) = break (==':') s
   in  case prt of
          ""       -> Just (host, Nothing)
          ':':port -> readM port >>= \p -> Just (host, Just p)
          _        -> Nothing
