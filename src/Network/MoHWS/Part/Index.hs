{- |
Copyright: 2002, Simon Marlow.
Copyright: 2006, Bjorn Bringert.
Copyright: 2009, Henning Thielemann.

Show @index.html@ or another configured file
whenever the URI path is a directory.
However, this module gets only active
if the directory path is terminated with a slash.
Without a slash the relative paths will not be processed correct by the web clients
(they will consider relative paths as relative to the superdirectory).
See also "Network.MoHWS.Part.AddSlash".
-}
module Network.MoHWS.Part.Index (Configuration, desc, ) where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Context as ServerContext
import Network.MoHWS.Logger.Error (debug, )

import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Configuration.Accessor as ConfigA
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.Basic ((.>))

import Network.MoHWS.Utility (statFile, hasTrailingSlash, )
import Data.Maybe (fromMaybe, )
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT, )
import Control.Monad.Trans.Class (lift, )
import Control.Monad (guard, )

import qualified System.FilePath as FilePath
import System.Posix (isDirectory, )



desc :: ModuleDesc.T body Configuration
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "index",
      ModuleDesc.load = return . funs,
      ModuleDesc.configParser = parser,
      ModuleDesc.setDefltConfig = const defltConfig
   }

data Configuration =
   Configuration {
      index_ :: String
   }

defltConfig :: Configuration
defltConfig =
   Configuration {
      index_ = "index.html"
   }

index :: Accessor.T Configuration String
index =
   Accessor.fromSetGet (\x c -> c{index_ = x}) index_

parser :: ConfigParser.T st Configuration
parser =
   ConfigParser.field "directoryindex" p_index

p_index :: ConfigParser.T st Configuration
p_index =
   ConfigParser.set (ConfigA.extension .> index) $ ConfigParser.stringLiteral

funs :: ServerContext.T Configuration -> Module.T body
funs st =
   Module.empty {
      Module.tweakRequest = tweakRequest st
   }

tweakRequest :: ServerContext.T Configuration -> ServerRequest.T body -> IO (ServerRequest.T body)
tweakRequest = Module.tweakFilename fixPath

fixPath :: ServerContext.T Configuration -> FilePath -> IO FilePath
fixPath st filename =
  let conf = ServerContext.config st
  in  fmap (fromMaybe filename) $
      runMaybeT $
      do guard (hasTrailingSlash filename)
         stat <- statFile filename
         guard (isDirectory stat)
         let indexFilename = FilePath.combine filename $ index_ $ Config.extension conf
         lift $ debug st $ "indexFilename = " ++ show indexFilename
         _ <- statFile indexFilename -- check whether file exists
         return indexFilename
