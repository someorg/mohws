{- |
Copyright: 2002, Simon Marlow.
Copyright: 2006, Bjorn Bringert.
Copyright: 2009, Henning Thielemann.
-}
module Network.MoHWS.Part.UserDirectory (Configuration, desc, ) where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Server.Context as ServerContext
import Network.MoHWS.Logger.Error (debug, )

import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Configuration.Accessor as ConfigA
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.Basic ((.>))

import Control.Monad (mzero, guard, )
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), )

import System.IO.Error (catchIOError, )
import System.Posix (homeDirectory, getUserEntryForName, )


desc :: ModuleDesc.T body Configuration
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "userdirectory",
      ModuleDesc.load = return . funs,
      ModuleDesc.configParser = parser,
      ModuleDesc.setDefltConfig = const defltConfig
   }

data Configuration =
   Configuration {
      userDir_ :: String
   }

defltConfig :: Configuration
defltConfig =
   Configuration {
      userDir_ = ""
   }

userDir :: Accessor.T Configuration String
userDir =
   Accessor.fromSetGet (\x c -> c{userDir_ = x}) userDir_

parser :: ConfigParser.T st Configuration
parser =
   ConfigParser.field "userdirectory" p_userDir

p_userDir :: ConfigParser.T st Configuration
p_userDir =
   ConfigParser.set (ConfigA.extension .> userDir) $ ConfigParser.stringLiteral

funs :: ServerContext.T Configuration -> Module.T body
funs st =
   Module.empty {
      Module.translatePath = translatePath st
   }

translatePath :: ServerContext.T Configuration -> String -> String -> MaybeT IO FilePath
translatePath st _host ('/':'~':userpath) =
  do let conf = ServerContext.config st
         (usr, path) = break (=='/') userpath
         dir = userDir_ $ Config.extension conf
     guard $ not $ null $ dir
     debug st $ "looking for user: " ++ show usr
     ent <-
        MaybeT $ flip catchIOError (const $ return Nothing) $
        fmap Just (getUserEntryForName usr)
     let p = '/': homeDirectory ent ++ '/':dir ++ path
     debug st $ "userdir path: " ++ p
     return p
translatePath _ _ _ = mzero
