-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
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

module Network.MoHWS.Utility where

import Control.Exception (try, catchJust, )

import Control.Concurrent (newEmptyMVar, takeMVar, )
import Control.Monad (liftM, )
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, )
import Data.Tuple.HT (mapSnd, )
import Data.List (intersperse, )
import Data.List.HT (switchL, switchR, maybePrefixOf, dropWhileRev, takeWhileRev, inits, tails, )
import Data.Ratio (numerator, )
import Foreign.C.Error (getErrno, eNOENT, eNOTDIR, )
import Network.Socket as Socket
import System.IO
import System.Exit (exitFailure, )
import System.Locale (defaultTimeLocale, )
import System.Posix (EpochTime, FileStatus,
          getFileStatus, getSymbolicLinkStatus, isSymbolicLink, )
import System.Time (CalendarTime, formatCalendarTime, ClockTime(TOD), )



-----------------------------------------------------------------------------
-- Utils

-- ToDo: deHex is supposed to remove the '%'-encoding
deHex :: String -> String
deHex s = s

hPutStrCrLf :: Handle -> String -> IO ()
hPutStrCrLf h s = hPutStr h s >> hPutChar h '\r' >> hPutChar h '\n'

die :: String -> IO ()
die err = do hPutStrLn stderr err
             exitFailure

-----------------------------------------------------------------------------
-- String utils

readM :: (Read a, Monad m) => String -> m a
readM s = readSM reads s

readSM :: Monad m => ReadS a -> String -> m a
readSM f s =
   case f s of
      [] -> fail $ "No parse of " ++ show s
      [(x,[])] -> return x
      [(_,_)]  -> fail $ "Junk at end of " ++ show s
      _  -> fail $ "Ambiguous parse of " ++ show s


-----------------------------------------------------------------------------
-- List utils

-- Split a list at some delimiter.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f =
   let recourse =
          uncurry (:) .
          mapSnd (switchL [] (const recourse)) .
          break f
   in  recourse

-- now also known as intercalate
glue :: [a] -> [[a]] -> [a]
glue g = concat . intersperse g

splits :: [a] -> [([a],[a])]
splits xs = zip (inits xs) (tails xs)

dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix xs pref =
   fromMaybe xs $ maybePrefixOf pref xs

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix xs suf = reverse (reverse xs `dropPrefix` reverse suf)

-----------------------------------------------------------------------------
-- File path utils

splitPath :: FilePath -> [String]
splitPath = splitBy (=='/')

joinPath :: [String] -> FilePath
joinPath = glue "/"

-- Get the directory component of a path
-- FIXME: is this good enough?
dirname :: FilePath -> FilePath
dirname = dropWhileRev (/= '/')

-- Get the filename component of a path
-- FIXME: probably System.FilePath should be used here.
basename :: FilePath -> FilePath
basename = takeWhileRev (/= '/')

hasTrailingSlash :: FilePath -> Bool
hasTrailingSlash =
   switchR False (\_ -> ('/'==))


-----------------------------------------------------------------------------
-- Time utils

formatTimeSensibly :: CalendarTime -> String
formatTimeSensibly time
   = formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" time

epochTimeToClockTime :: EpochTime -> ClockTime
epochTimeToClockTime epoch_time = TOD (numToInteger epoch_time) 0
  where numToInteger = numerator . toRational

-----------------------------------------------------------------------------
-- concurrency utilities

-- block forever
wait :: IO a
wait = newEmptyMVar >>= takeMVar

-----------------------------------------------------------------------------
-- networking utils

accept :: Socket                -- Listening Socket
       -> IO (Handle,SockAddr)  -- StdIO Handle for read/write
accept sock = do
 (sock', addr) <- Socket.accept sock
 hndle <- socketToHandle sock' ReadWriteMode
 return (hndle,addr)

-----------------------------------------------------------------------------
-- file utils

statFile :: String -> MaybeT IO FileStatus
statFile = stat_ getFileStatus

statSymLink :: String -> MaybeT IO FileStatus
statSymLink = stat_ getSymbolicLinkStatus

stat_ :: (FilePath -> IO FileStatus) -> String -> MaybeT IO FileStatus
stat_ f filename = MaybeT $ do
  maybe_stat <- try (f filename)
  case maybe_stat of
       Left e -> do
          errno <- getErrno
          if errno == eNOENT || errno == eNOTDIR
             then return Nothing
             else ioError e
       Right stat ->
          return $ Just stat

isSymLink :: FilePath -> IO Bool
isSymLink = liftM (maybe False isSymbolicLink) . runMaybeT . statSymLink

-----------------------------------------------------------------------------
-- Exception utils

-- | Catch IO Errors for which a given predicate is true.
catchSomeIOErrors :: (IOError -> Bool) -> IO a -> (IOError -> IO a) -> IO a
catchSomeIOErrors p =
   catchJust (\e -> toMaybe (p e) e)
