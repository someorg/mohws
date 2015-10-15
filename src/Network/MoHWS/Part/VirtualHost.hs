-- Copyright 2009, Henning Thielemann
module Network.MoHWS.Part.VirtualHost
          (Configuration, desc,
           virtualDocumentRoot, virtualFile, ) where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Server.Context as ServerContext
import qualified System.FilePath as FilePath

import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Configuration.Accessor as ConfigA
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.Basic ((.>))
import qualified Text.ParserCombinators.Parsec as Parsec

import Network.Socket (HostName, )

import qualified Data.Map as Map
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), )
import Control.Monad (mplus, )


desc :: ModuleDesc.T body Configuration
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "virtualhost",
      ModuleDesc.load = return . funs,
      ModuleDesc.configParser = parser,
      ModuleDesc.setDefltConfig = const defltConfig
   }

data Configuration =
   Configuration {
      virtualDocumentRoot_   :: Map.Map HostName FilePath,
      virtualFile_           :: Map.Map HostName (Map.Map String FilePath)
   }

defltConfig :: Configuration
defltConfig =
   Configuration {
      virtualDocumentRoot_   = Map.empty,
      virtualFile_           = Map.empty
   }

virtualDocumentRoot :: Accessor.T Configuration (Map.Map HostName FilePath)
virtualDocumentRoot =
   Accessor.fromSetGet (\x c -> c{virtualDocumentRoot_ = x}) virtualDocumentRoot_

virtualFile :: Accessor.T Configuration (Map.Map HostName (Map.Map String FilePath))
virtualFile =
   Accessor.fromSetGet (\x c -> c{virtualFile_ = x}) virtualFile_

parser :: ConfigParser.T st Configuration
parser =
   Parsec.choice $
   (ConfigParser.field "virtualdocumentroot"    p_virtualDocumentRoot) :
   (ConfigParser.field "virtualfile"            p_virtualFile) :
   []

p_virtualDocumentRoot :: ConfigParser.T st Configuration
p_virtualDocumentRoot =
   do host <- ConfigParser.stringLiteral
      root <- ConfigParser.stringLiteral
      return $
         Accessor.modify (ConfigA.extension .> virtualDocumentRoot)
            (Map.insert host root)

p_virtualFile :: ConfigParser.T st Configuration
p_virtualFile =
   do host        <- ConfigParser.stringLiteral
      virtualPath <- ConfigParser.stringLiteral
      realPath    <- ConfigParser.stringLiteral
      return $
         Accessor.modify (ConfigA.extension .> virtualFile)
            (Map.insertWith Map.union host (Map.singleton virtualPath realPath))

funs :: ServerContext.T Configuration -> Module.T body
funs st =
   Module.empty {
      Module.isServerHost  = isServerHost st,
      Module.translatePath = translatePath st
   }

{- |
In earlier versions we did just add the virtual hosts to the ServerAliases
in the configuration step.
I think this solution is cleaner.
-}
isServerHost :: ServerContext.T Configuration -> HostName -> Bool
isServerHost st host =
   let ext = Config.extension $ ServerContext.config st
   in  Map.member host (virtualFile_ ext) ||
       Map.member host (virtualDocumentRoot_ ext)

translatePath :: ServerContext.T Configuration -> String -> String -> MaybeT IO FilePath
translatePath st host path =
--   (\x -> print (host,path) >> print x >> return x) $
   MaybeT $ return $
   let conf = ServerContext.config st
       ext  = Config.extension conf
   in  mplus
          (fmap (FilePath.combine (Config.documentRoot conf)) $
           Map.lookup path =<< Map.lookup host (virtualFile_ ext))
          (case path of
              '/':_ ->
                 fmap (++path) $
--                 fmap (flip FilePath.combine path) $  this omits the trailing slash when path=="/"
                 Map.lookup host (virtualDocumentRoot_ ext)
              _ -> Nothing)
