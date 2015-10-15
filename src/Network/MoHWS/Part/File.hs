{- |
Copyright: 2002, Simon Marlow.
Copyright: 2006, Bjorn Bringert.
Copyright: 2009, Henning Thielemann.
-}
module Network.MoHWS.Part.File (Configuration, desc, ) where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Request as Request
import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Stream as Stream
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Context as ServerContext
import Network.MoHWS.Logger.Error (abort, debugOnAbort, )
import Network.MoHWS.Utility (statFile, statSymLink, epochTimeToClockTime, )
import qualified System.IO as IO

import Data.Bool.HT (if', )
import Control.Monad.Trans.Maybe (MaybeT, )
import Control.Monad.Trans.Class (lift, )
import System.Posix (isRegularFile, isSymbolicLink,
          FileStatus, fileAccess, modificationTime, fileSize, )

desc :: (Stream.C body) => ModuleDesc.T body Configuration
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "file",
      ModuleDesc.load = return . funs,
      ModuleDesc.setDefltConfig = const defltConfig
   }

{- |
Dummy Configuration that forces users
to use the lifting mechanism,
which in turn asserts that future extensions are respected.
-}
data Configuration =
   Configuration {
   }

defltConfig :: Configuration
defltConfig =
   Configuration {
   }


funs :: (Stream.C body) =>
   ServerContext.T ext -> Module.T body
funs st =
   Module.empty {
      Module.handleRequest = handleRequest st
   }

handleRequest :: (Stream.C body) =>
   ServerContext.T ext -> ServerRequest.T body  -> MaybeT IO (Response.T body)
handleRequest st
     (ServerRequest.Cons {
        ServerRequest.clientRequest = req,
        ServerRequest.serverFilename = filename
      }) =
   let conf = ServerContext.config st
       processFile =
          do fstat <- statFile filename
             lift $
                case Request.command req of
                   Request.GET  -> serveFile st filename fstat False
                   Request.HEAD -> serveFile st filename fstat True
                   _ -> return (Response.makeNotImplemented conf)
       checkStat stat =
          if' (isRegularFile stat) processFile $
          if' (isSymbolicLink stat)
             (if Config.followSymbolicLinks conf
                then processFile
                else abort st $ "findFile: Not following symlink: " ++ show filename) $
          (abort st $ "Strange file: " ++ show filename)
   in  debugOnAbort st ("File not found: " ++ show filename)
          (statSymLink filename) >>=
       checkStat

serveFile :: (Stream.C body) =>
   ServerContext.T ext -> FilePath -> FileStatus -> Bool -> IO (Response.T body)
serveFile st filename stat is_head =
   do
     let conf = ServerContext.config st
     -- check we can actually read this file
     access <- fileAccess filename True{-read-} False False
     case access of
       False -> return (Response.makeNotFound conf)
         -- not "permission denied", we're being paranoid about security.
       True ->
         do let contentType = ServerContext.getMimeType st filename

            let lastModified = epochTimeToClockTime (modificationTime stat)

            let size = toInteger (fileSize stat)

            h <- IO.openFile filename IO.ReadMode
            content <- Stream.readAll (Config.chunkSize conf) h

            let body =
                   Response.Body {
                      Response.size = Just size,
                      Response.source = filename,
                      Response.close = IO.hClose h,
                      Response.content = content
                   }

            return $
               Response.makeOk conf
                  (not is_head) {- send body -}
                  (Header.group
                     [Header.makeContentType contentType,
                      Header.makeLastModified lastModified])
                  body
