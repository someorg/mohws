-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
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

module Network.MoHWS.Configuration where

import qualified Data.Set as Set
import qualified Network.MoHWS.Logger.Level as LogLevel
import qualified Data.Accessor.Basic as Accessor
import Network.Socket (PortNumber, )

import qualified Data.List as List
import qualified Data.Version as Ver
import qualified Paths_mohws as Global


-----------------------------------------------------------------------------
-- Config info

data T ext = Cons {
  user                  :: String,
  group                 :: String,

  listen                :: [(Maybe String, PortNumber)],

  requestTimeout        :: Int,
  keepAliveTimeout      :: Int,
  maxClients            :: Int,

  serverAdmin           :: String,      -- "" indicates no admin
  serverName            :: String,      -- "" indicates no canon name
  serverAlias           :: Set.Set String,
  useCanonicalName      :: Bool,
  hostnameLookups       :: Bool,

  documentRoot          :: FilePath,
  accessFileName        :: FilePath,
  indexes               :: Bool,
  followSymbolicLinks   :: Bool,
  chunkSize             :: Int,

  typesConfig           :: String,
  defaultType           :: String,

  addLanguage           :: [(String,String)],
  languagePriority      :: [String],

  customLogs            :: [(FilePath, String)],

  errorLogFile          :: FilePath,
  logLevel              :: LogLevel.T,

  extension             :: ext
  }
  deriving Show

deflt :: ext -> T ext
deflt ext = Cons {
  user = "nobody",
  group = "nobody",

  listen                = [(Nothing,80)],

  requestTimeout        = 300,
  keepAliveTimeout      = 15,
  maxClients            = 150,

  serverAdmin           = "",
  serverName            = "",
  serverAlias           = Set.empty,
  useCanonicalName      = False,
  hostnameLookups       = False,

  documentRoot          = ".",
  accessFileName        = ".htaccess",
  indexes               = False,
  followSymbolicLinks   = False,
  chunkSize             = 4096,

  typesConfig           = "/etc/mime.types",
  defaultType           = "text/plain",

  addLanguage           = [],
  languagePriority      = [],

  customLogs            = [("http-access.log",
                            "%h %l %u %t \"%r\" %s %b \"%{Referer}i\" \"%{User-Agent}i\"")],

  errorLogFile          = "httpd-error.log",
  logLevel              = LogLevel.Warn,

  extension             = ext
  }

-- not user-definable...
serverSoftware, serverVersion :: String
serverSoftware = "MoHWS"
serverVersion  =
   concat $ List.intersperse "." $ map show $
   Ver.versionBranch Global.version


extensionAcc :: Accessor.T (T ext) ext
extensionAcc =
   Accessor.fromSetGet (\e c -> c{extension=e}) extension


instance Functor T where
   fmap f c = Cons {
      user                  = user c,
      group                 = group c,

      listen                = listen c,

      requestTimeout        = requestTimeout c,
      keepAliveTimeout      = keepAliveTimeout c,
      maxClients            = maxClients c,

      serverAdmin           = serverAdmin c,
      serverName            = serverName c,
      serverAlias           = serverAlias c,
      useCanonicalName      = useCanonicalName c,
      hostnameLookups       = hostnameLookups c,

      documentRoot          = documentRoot c,
      accessFileName        = accessFileName c,
      indexes               = indexes c,
      followSymbolicLinks   = followSymbolicLinks c,
      chunkSize             = chunkSize c,

      typesConfig           = typesConfig c,
      defaultType           = defaultType c,

      addLanguage           = addLanguage c,
      languagePriority      = languagePriority c,

      customLogs            = customLogs c,

      errorLogFile          = errorLogFile c,
      logLevel              = logLevel c,

      extension             = f $ extension c
   }
