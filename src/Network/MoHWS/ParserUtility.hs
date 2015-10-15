-- HTTP parsing utilities
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
module Network.MoHWS.ParserUtility where

import Network.MoHWS.Utility (splitBy, )

import Control.Monad (liftM2, )
import Data.Char (chr, )
import Data.List ((\\), )
import Data.List.HT (dropWhileRev, )
import System.IO (Handle, hGetLine, )
import Text.ParserCombinators.Parsec
          (GenParser, Parser, char, string,
           (<|>), try, oneOf, noneOf, option, skipMany, many, many1, )


pSP :: Parser Char
pSP = char ' '

-- | RFC 822 LWSP-char
pWS1 :: Parser Char
pWS1 = oneOf " \t"

crLf :: String
crLf = "\r\n"

-- | RFC 2616 CRLF
pCRLF :: Parser String
pCRLF = try (string "\r\n" <|> string "\n\r") <|> string "\n" <|> string "\r"

lexeme :: Parser a -> Parser a
lexeme p = do x <- p; skipMany pWS1; return x

-- | One line
lineString :: Parser String
lineString = many (noneOf "\n\r")

headerNameChar :: Parser Char
headerNameChar = noneOf "\n\r:"

especials, tokenchar :: [Char]
especials = "()<>@,;:\\\"/[]?.="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ especials

pToken :: Parser String
pToken = many1 (oneOf tokenchar)

textChars :: [Char]
textChars = map chr ([1..9] ++ [11,12] ++ [14..127])

pText :: Parser Char
pText = oneOf textChars


-- parse the list format described in RFC 2616, section 2.1
parseList :: String -> [String]
parseList = map trimLWS . splitBy (==',')

dropLeadingLWS :: String -> String
dropLeadingLWS = dropWhile isLWSChar

trimLWS :: String -> String
trimLWS = dropWhileRev isLWSChar . dropLeadingLWS

isLWSChar :: Char -> Bool
isLWSChar c = c == ' ' || c == '\t'


-- Read input up to the first empty line
getUntilEmptyLine :: Handle -> IO String
getUntilEmptyLine h =
    do l <- hGetLine h
       if emptyLine l
         then return "\n"
         else getUntilEmptyLine h >>= return . ((l++) . ('\n':))

emptyLine :: String -> Bool
emptyLine "\r" = True
emptyLine ""   = True
emptyLine _    = False


countBetween :: Int -> Int -> GenParser tok st a -> GenParser tok st [a]
countBetween 0 0 _ = return []
countBetween 0 ma p = option [] (liftM2 (:) p (countBetween 0 (ma-1) p))
countBetween mi ma p = liftM2 (:) p (countBetween (mi-1) (ma-1) p)
