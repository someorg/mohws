{- |
Copyright: 2009, Henning Thielemann
-}
module Network.MoHWS.Configuration.Accessor where

import qualified Network.MoHWS.Configuration as Config
import qualified Data.Set as Set
import qualified Data.Accessor.Basic as Accessor

import qualified Network.MoHWS.Logger.Level as LogLevel
import Network.Socket (PortNumber, )

{-
(\w+)( *):: (.+)
\1 :: Accessor.T (Config.T ext) \3\n\1 =\n   Accessor.fromSetGet (\\x c -> c{Config.\1 = x}) Config.\1\n
-}

user :: Accessor.T (Config.T ext) String
user =
   Accessor.fromSetGet (\x c -> c{Config.user = x}) Config.user

group :: Accessor.T (Config.T ext) String
group =
   Accessor.fromSetGet (\x c -> c{Config.group = x}) Config.group


listen :: Accessor.T (Config.T ext) [(Maybe String, PortNumber)]
listen =
   Accessor.fromSetGet (\x c -> c{Config.listen = x}) Config.listen


requestTimeout :: Accessor.T (Config.T ext) Int
requestTimeout =
   Accessor.fromSetGet (\x c -> c{Config.requestTimeout = x}) Config.requestTimeout

keepAliveTimeout :: Accessor.T (Config.T ext) Int
keepAliveTimeout =
   Accessor.fromSetGet (\x c -> c{Config.keepAliveTimeout = x}) Config.keepAliveTimeout

maxClients :: Accessor.T (Config.T ext) Int
maxClients =
   Accessor.fromSetGet (\x c -> c{Config.maxClients = x}) Config.maxClients


serverAdmin :: Accessor.T (Config.T ext) String
serverAdmin =
   Accessor.fromSetGet (\x c -> c{Config.serverAdmin = x}) Config.serverAdmin

serverName :: Accessor.T (Config.T ext) String
serverName =
   Accessor.fromSetGet (\x c -> c{Config.serverName = x}) Config.serverName

serverAlias :: Accessor.T (Config.T ext) (Set.Set String)
serverAlias =
   Accessor.fromSetGet (\x c -> c{Config.serverAlias = x}) Config.serverAlias

useCanonicalName :: Accessor.T (Config.T ext) Bool
useCanonicalName =
   Accessor.fromSetGet (\x c -> c{Config.useCanonicalName = x}) Config.useCanonicalName

hostnameLookups :: Accessor.T (Config.T ext) Bool
hostnameLookups =
   Accessor.fromSetGet (\x c -> c{Config.hostnameLookups = x}) Config.hostnameLookups


documentRoot :: Accessor.T (Config.T ext) FilePath
documentRoot =
   Accessor.fromSetGet (\x c -> c{Config.documentRoot = x}) Config.documentRoot

accessFileName :: Accessor.T (Config.T ext) FilePath
accessFileName =
   Accessor.fromSetGet (\x c -> c{Config.accessFileName = x}) Config.accessFileName

indexes :: Accessor.T (Config.T ext) Bool
indexes =
   Accessor.fromSetGet (\x c -> c{Config.indexes = x}) Config.indexes

followSymbolicLinks :: Accessor.T (Config.T ext) Bool
followSymbolicLinks =
   Accessor.fromSetGet (\x c -> c{Config.followSymbolicLinks = x}) Config.followSymbolicLinks

chunkSize :: Accessor.T (Config.T ext) Int
chunkSize =
   Accessor.fromSetGet (\x c -> c{Config.chunkSize = x}) Config.chunkSize


typesConfig :: Accessor.T (Config.T ext) String
typesConfig =
   Accessor.fromSetGet (\x c -> c{Config.typesConfig = x}) Config.typesConfig

defaultType :: Accessor.T (Config.T ext) String
defaultType =
   Accessor.fromSetGet (\x c -> c{Config.defaultType = x}) Config.defaultType


addLanguage :: Accessor.T (Config.T ext) [(String,String)]
addLanguage =
   Accessor.fromSetGet (\x c -> c{Config.addLanguage = x}) Config.addLanguage

languagePriority :: Accessor.T (Config.T ext) [String]
languagePriority =
   Accessor.fromSetGet (\x c -> c{Config.languagePriority = x}) Config.languagePriority


customLogs :: Accessor.T (Config.T ext) [(FilePath, String)]
customLogs =
   Accessor.fromSetGet (\x c -> c{Config.customLogs = x}) Config.customLogs


errorLogFile :: Accessor.T (Config.T ext) FilePath
errorLogFile =
   Accessor.fromSetGet (\x c -> c{Config.errorLogFile = x}) Config.errorLogFile

logLevel :: Accessor.T (Config.T ext) LogLevel.T
logLevel =
   Accessor.fromSetGet (\x c -> c{Config.logLevel = x}) Config.logLevel


extension :: Accessor.T (Config.T ext) ext
extension =
   Accessor.fromSetGet (\x c -> c{Config.extension = x}) Config.extension

