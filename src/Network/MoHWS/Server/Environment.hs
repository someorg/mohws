{- |
Copyright: 2006, Bjorn Bringert
Copyright: 2009, Henning Thielemann

This is an extension of ServerContext,
which is used privately in the Server.
In addition to ServerContext it holds the module list,
which is not accessible by modules.
-}
module Network.MoHWS.Server.Environment where

import qualified Network.MoHWS.Server.Context as ServerContext
import qualified Network.MoHWS.Server.Options as Options
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Logger.Access as AccessLogger
import qualified Network.MoHWS.Logger.Error as ErrorLogger
import qualified Network.MoHWS.HTTP.MimeType as MimeType
import qualified Network.MoHWS.HTTP.Response as Response

import Control.Monad (foldM, msum, )
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT, )
import Network.BSD (HostEntry, )
import Network.Socket (PortNumber, )
import System.Time (TimeDiff, )


data T body ext = Cons
   {
      context :: ServerContext.T ext,
      port :: PortNumber,
      modules :: [Module.T body]
   }

-- * Read accessors

options :: T body ext -> Options.T
options = ServerContext.options . context

config :: T body ext -> Config.T ext
config = ServerContext.config . context

hostName :: T body ext -> HostEntry
hostName = ServerContext.hostName . context

mimeTypes :: T body ext -> MimeType.Dictionary
mimeTypes = ServerContext.mimeTypes . context

errorLogger :: T body ext -> ErrorLogger.Handle
errorLogger = ServerContext.errorLogger . context

accessLoggers :: T body ext -> [AccessLogger.Handle]
accessLoggers = ServerContext.accessLoggers . context


-- * Loggers

instance ErrorLogger.HasHandle (T body ext) where
   getHandle = errorLogger

logAccess :: T body ext -> ServerRequest.T body -> Response.T body -> TimeDiff -> IO ()
logAccess = ServerContext.logAccess . context



-- * Modules

mapModules_ :: T body ext -> (Module.T body -> IO ()) -> IO ()
mapModules_ st f = mapM_ f (modules st)

foldModules :: T body ext -> (Module.T body -> a -> IO a) -> a -> IO a
foldModules st f x = foldM (flip f) x (modules st)

tryModules :: T body ext -> (Module.T body -> MaybeT IO a) -> IO (Maybe a)
tryModules st f = runMaybeT $ msum $ map f $ modules st
