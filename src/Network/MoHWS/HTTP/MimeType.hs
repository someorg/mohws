-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
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

module Network.MoHWS.HTTP.MimeType (
   Dictionary,
   T(Cons),
   loadDictionary,
   fromFileName,
   ) where

import Network.MoHWS.ParserUtility

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
          (Parser, parse, char, spaces, sepBy, )
import qualified System.FilePath as FilePath
import Control.Monad (liftM2, guard, )
import Data.Maybe (mapMaybe, )
import Data.List.HT (viewL, )


type Dictionary = Map String T

data T = Cons String String

instance Show T where
   showsPrec _ (Cons part1 part2) = showString (part1 ++ '/':part2)

fromFileName :: Dictionary -> FilePath -> Maybe T
fromFileName mime_types filename =
   do (sep,ext) <- viewL $ FilePath.takeExtension filename
      guard (FilePath.isExtSeparator sep)
      guard (not $ null ext)
      Map.lookup ext mime_types

loadDictionary :: FilePath -> IO Dictionary
loadDictionary mime_types_file =
   fmap (Map.fromList . parseDictionary) $
   readFile mime_types_file

parseDictionary :: String -> [(String,T)]
parseDictionary file =
   do (val,exts) <- mapMaybe (parseMimeLine . takeWhile (/= '#')) (lines file)
      ext <- exts
      return (ext,val)

parseMimeLine :: String -> Maybe (T, [String])
parseMimeLine l =
   case parse parserLine "MIME line" l of
      Left _  -> Nothing
      Right m -> Just m

parserLine :: Parser (T, [String])
parserLine =
   liftM2 (,) parser (spaces >> sepBy pToken spaces)

parser :: Parser T
parser =
   liftM2 Cons pToken (char '/' >> pToken)
