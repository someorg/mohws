{- |
Copyright: 2006, Bjorn Bringert
Copyright: 2009, Henning Thielemann
-}
module Network.MoHWS.Server.Context where

import qualified Network.MoHWS.Server.Options as Options
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Logger.Access as AccessLogger
import qualified Network.MoHWS.Logger.Error as ErrorLogger
import qualified Network.MoHWS.HTTP.MimeType as MimeType
import qualified Network.MoHWS.HTTP.Response as Response

import Network.BSD (HostEntry, )
import System.Time (TimeDiff, )


-- * ServerContext

data T ext = Cons
   {
      options :: Options.T,
      config :: Config.T ext,
      hostName :: HostEntry,
      mimeTypes :: MimeType.Dictionary,
      errorLogger :: ErrorLogger.Handle,
      accessLoggers :: [AccessLogger.Handle]
   }

instance Functor T where
   fmap f st = Cons {
      options = options st,
      config = fmap f $ config st,
      hostName = hostName st,
      mimeTypes = mimeTypes st,
      errorLogger = errorLogger st,
      accessLoggers = accessLoggers st
   }


-- * MIME types

getMimeType :: T ext -> FilePath -> String
getMimeType st filename =
   let def = Config.defaultType (config st)
   in  maybe def show (MimeType.fromFileName (mimeTypes st) filename)

-- * Logging

instance ErrorLogger.HasHandle (T ext) where
   getHandle = errorLogger

logAccess :: T ext -> ServerRequest.T body -> Response.T body -> TimeDiff -> IO ()
logAccess st req resp delay =
    do msg <- AccessLogger.mkRequest req resp (hostName st) delay
       mapM_ (\l -> AccessLogger.log l msg) (accessLoggers st)
