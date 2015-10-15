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

module Network.MoHWS.Configuration.Parser (
   T, lift, run, field, set, addToList,
   stringLiteral, bool, int,
   ) where

import qualified Network.MoHWS.Configuration.Accessor as ConfigA
import qualified Network.MoHWS.Configuration as Config
import Network.MoHWS.ParserUtility (countBetween, )
import Control.Monad (liftM2, )
import Network.MoHWS.Utility (readM, )

import Text.ParserCombinators.Parsec
         (GenParser, ParseError, parseFromFile,
          (<|>), choice, many, option, try,
          char, digit, eof, )
import Text.ParserCombinators.Parsec.Language
         (LanguageDef, emptyDef, commentLine, nestedComments,
          reservedOpNames, reservedNames, caseSensitive, )
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Set as Set
import qualified Data.Accessor.Basic as Accessor


type T st ext = GenParser Char st (Builder ext)

type Builder ext = Config.T ext -> Config.T ext


{-
lift ::
   Accessor.T fullExt partExt ->
   GenParser Char st (partExt -> partExt) -> T st fullExt
lift act =
   fmap (Accessor.modify Config.extensionAcc . Accessor.modify act)
-}

lift ::
   Accessor.T fullExt partExt ->
   T st partExt -> T st fullExt
lift act =
   fmap (\build c ->
      fmap (flip (Accessor.set act) (Config.extension c)) $
      build $
      fmap (Accessor.get act) c)


field :: String -> T st ext -> T st ext
field keyword parser =
   Token.reserved p keyword >> parser

p :: Token.TokenParser st
p = Token.makeTokenParser tokenDef


stringLiteral :: GenParser Char st String
stringLiteral = Token.stringLiteral p

bool :: GenParser Char st Bool
bool = ( Token.reserved p "On"  >> return True )
   <|> ( Token.reserved p "Off" >> return False )

int :: GenParser Char st Int
int = fmap fromInteger $ Token.integer p

tokenDef :: LanguageDef st
tokenDef =
   emptyDef {
      commentLine     = "#",
      nestedComments  = False,
      reservedOpNames = [],
      reservedNames   = [],
      caseSensitive   = False
   }


run :: T () ext -> String -> IO (Either ParseError (Builder ext))
run parseExt fname =
   parseFromFile (configParser parseExt) fname

configParser :: T st ext -> T st ext
configParser parseExt = do
   Token.whiteSpace p
   cs <- many $ parseExt <|> configLine
   eof
   return (fixConfig . foldr (.) id cs)

fixConfig :: Builder ext
fixConfig conf =
   let f xs = if length xs > 1 then init xs else xs
   in  Accessor.modify ConfigA.listen f conf

configLine :: T st ext
configLine =
   choice $
   (field "user"                   p_user) :
   (field "group"                  p_group) :
   (field "timeout"                p_timeout) :
   (field "keepalivetimeout"       p_keepAliveTimeout) :
   (field "maxclients"             p_maxClients) :
   (field "listen"                 p_listen) :
   (field "serveradmin"            p_serverAdmin) :
   (field "servername"             p_serverName) :
   (field "serveralias"            p_serverAlias) :
   (field "usecanonicalname"       p_useCanonicalName) :
   (field "documentroot"           p_documentRoot) :
   (field "accessfilename"         p_accessFileName) :
   (field "followsymboliclinks"    p_followSymbolicLinks) :
   (field "chunksize"              p_chunkSize) :
   (field "typesconfig"            p_typesConfig) :
   (field "defaulttype"            p_defaultType) :
   (field "hostnamelookups"        p_hostnameLookups) :
   (field "errorlog"               p_errorLog) :
   (field "loglevel"               p_logLevel) :
   (field "customlog"              p_customLog) :
   (field "listen"                 p_listen) :
   (field "addlanguage"            p_addLanguage) :
   (field "languagepriority"       p_languagePriority) :
   []

set :: Accessor.T r a -> GenParser Char st a -> GenParser Char st (r -> r)
set acc = fmap (Accessor.set acc)

addToList :: Accessor.T r [a] -> GenParser Char st a -> GenParser Char st (r -> r)
addToList acc = fmap (Accessor.modify acc . (:))


p_user :: T st ext
p_user  = set ConfigA.user $ stringLiteral
p_group :: T st ext
p_group = set ConfigA.group $ stringLiteral
p_timeout :: T st ext
p_timeout = set ConfigA.requestTimeout $ int
p_keepAliveTimeout :: T st ext
p_keepAliveTimeout = set ConfigA.keepAliveTimeout $ int
p_maxClients :: T st ext
p_maxClients  = set ConfigA.maxClients $ int
p_serverAdmin :: T st ext
p_serverAdmin = set ConfigA.serverAdmin $ stringLiteral
p_serverName :: T st ext
p_serverName = set ConfigA.serverName $ stringLiteral
p_serverAlias :: T st ext
p_serverAlias = fmap (Accessor.modify ConfigA.serverAlias . Set.insert) $ stringLiteral
p_useCanonicalName :: T st ext
p_useCanonicalName = set ConfigA.useCanonicalName $ bool
p_documentRoot :: T st ext
p_documentRoot = set ConfigA.documentRoot $ stringLiteral

p_accessFileName :: T st ext
p_accessFileName = set ConfigA.accessFileName $ stringLiteral
p_followSymbolicLinks :: T st ext
p_followSymbolicLinks = set ConfigA.followSymbolicLinks $ bool
p_chunkSize :: T st ext
p_chunkSize = set ConfigA.chunkSize $ int
p_typesConfig :: T st ext
p_typesConfig = set ConfigA.typesConfig $ stringLiteral
p_defaultType :: T st ext
p_defaultType = set ConfigA.defaultType $ stringLiteral

p_hostnameLookups :: T st ext
p_hostnameLookups = set ConfigA.hostnameLookups $ bool
p_errorLog :: T st ext
p_errorLog = set ConfigA.errorLogFile $ stringLiteral

p_logLevel :: T st ext
p_logLevel = set ConfigA.logLevel $ Token.identifier p >>= readM

p_customLog :: T st ext
p_customLog =
   addToList ConfigA.customLogs $
   liftM2 (,) stringLiteral stringLiteral

p_listen :: T st ext
p_listen =
   let p_addr =
          option Nothing $ try $
             do addr <- p_ip_addr
                _ <- char ':'
                return $ Just addr
       p_ip_addr =
          fmap concat $ sequence $
          p_dec_byte : replicate 3 p_dot_dec_byte
       p_dec_byte = countBetween 1 3 digit
       p_dot_dec_byte = liftM2 (:) (char '.') p_dec_byte

   in  addToList ConfigA.listen $
       liftM2 (,) p_addr (fmap fromInteger $ Token.integer p)


p_addLanguage :: T st ext
p_addLanguage =
   addToList ConfigA.addLanguage $
   liftM2 (,) stringLiteral stringLiteral

p_languagePriority :: T st ext
p_languagePriority = set ConfigA.languagePriority $ many stringLiteral
