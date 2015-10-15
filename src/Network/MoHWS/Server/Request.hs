-- Copyright 2006, Bjorn Bringert.
-- Copyright 2009, Henning Thielemann.
module Network.MoHWS.Server.Request where

import qualified Network.MoHWS.HTTP.Request as Request

import Network.BSD (HostEntry, )
import Network.Socket (HostAddress, PortNumber, )

-- | All the server's information about a request
data T body = Cons
  {
     clientRequest :: Request.T body,
     clientAddress :: HostAddress,
     clientName :: Maybe HostEntry,
     requestHostName :: HostEntry,
     serverURIPath :: String,
     serverFilename :: FilePath,
     serverPort :: PortNumber
  }
  deriving Show

instance Functor T where
   fmap f req =
      Cons {
         clientAddress   = clientAddress   req,
         clientName      = clientName      req,
         requestHostName = requestHostName req,
         serverURIPath   = serverURIPath   req,
         serverFilename  = serverFilename  req,
         serverPort      = serverPort      req,
         clientRequest   = fmap f $ clientRequest req
      }
