{- |
Copyright: 2009, Henning Thielemann

Unified interface to String and ByteStrings.
-}
module Network.MoHWS.Stream where

import Network.MoHWS.ParserUtility (crLf, )

import qualified System.IO as IO
import Numeric (showHex, )

import qualified Network.MoHWS.ByteString as BU
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

import qualified Data.List.HT as ListHT
import qualified Data.List    as List
import Data.Monoid (Monoid, )

import Prelude hiding (length, drop, )


class Monoid stream => C stream where
   fromString :: Int -> String -> stream
   toString :: stream -> String
   isEmpty :: stream -> Bool
   length :: stream -> Integer
   isPrefixOf :: stream -> stream -> Bool
   break :: (Char -> Bool) -> stream -> (stream, stream)
   drop :: Int -> stream -> stream
   read :: IO.Handle -> Integer -> IO stream
   readAll :: Int -> IO.Handle -> IO stream
   write :: IO.Handle -> stream -> IO ()
   writeChunked :: Int -> IO.Handle -> stream -> IO ()

class Eq char => CharType char where
   fromChar :: Char -> char
   toChar   :: char -> Char

instance CharType Char where
   fromChar = id
   toChar   = id

instance CharType char => C [char] where
   fromString _chunkSize = map fromChar
   toString = map toChar
   isEmpty = null
   length = fromIntegral . List.length
   isPrefixOf = List.isPrefixOf
   break p = List.break (p . toChar)
   drop = List.drop
   read h n = fmap (map fromChar) $ BU.hGetChars h (fromInteger n)
   readAll _chunkSize = fmap (map fromChar) . IO.hGetContents
   write h = IO.hPutStr h . map toChar
   writeChunked chunkSize h =
      IO.hPutStr h .
      foldr ($) "" .
      map (\chunk ->
              showHex (length chunk) . showString crLf .
              showString (map toChar chunk) . showString crLf) .
      ListHT.sliceVertical chunkSize

instance C BL.ByteString where
--   fromString = BL.pack
   fromString chunkSize =
      BL.fromChunks .
      map BS.pack .
      ListHT.sliceVertical chunkSize
   toString = BL.unpack
   isEmpty = BL.null
   length = fromIntegral . BL.length
   isPrefixOf = BL.isPrefixOf
   break = BL.break
   drop = BL.drop . fromIntegral
   read h n = BL.hGet h (fromInteger n)
   readAll = BU.hGetContentsN
   write = BL.hPut
   writeChunked _chunkSize h str =
      flip mapM_ (BL.toChunks str) $ \chunk ->
         do IO.hPutStr h $ showHex (BS.length chunk) crLf
            BS.hPut h chunk
            IO.hPutStr h $ crLf
