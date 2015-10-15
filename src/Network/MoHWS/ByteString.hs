module Network.MoHWS.ByteString where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified System.IO as IO
import Control.Monad (liftM2, )
import System.IO.Unsafe (unsafeInterleaveIO, )


{- |
Like 'L.hGetContents' but it does not try to close the file when reaching the end.
Since you may abort reading before reaching the end,
or the run-time system reads more than necessary
(you don't know how unsafeInterleaveIO works),
you never know, whether 'L.hGetContents' has already closed the file or not.
With this function however it is always sure,
that the file is not closed and you are responsible for closing it.
-}
hGetContentsN :: Int -> IO.Handle -> IO L.ByteString
hGetContentsN k h =
   let loop = unsafeInterleaveIO $ do
          eof <- IO.hIsEOF h
          if eof
            then return L.empty
            else
              do liftM2
                    (\c -> L.append (L.fromChunks [c]))
                    (S.hGet h k) loop
   in  loop

{- |
Variant of 'hGetContentsN' that may choose smaller chunks
when currently no more data is available.
The chunk size may however become arbitrarily small,
making the whole process inefficient.
But when real-time fetching counts, it is the better choice.
-}
hGetContentsNonBlockingN :: Int -> IO.Handle -> IO L.ByteString
hGetContentsNonBlockingN k h =
   let lazyRead = unsafeInterleaveIO loop

       loop = do
          c <- S.hGetNonBlocking h k
          if S.null c
            then do eof <- IO.hIsEOF h
                    if eof
                      then return L.empty
                      else IO.hWaitForInput h (-1) >> loop
            else fmap (L.append $ L.fromChunks [c]) lazyRead
   in  lazyRead

-- | Read the given number of bytes from a Handle
hGetChars :: IO.Handle -> Int -> IO String
hGetChars h n = fmap L.unpack $ L.hGet h n
