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

module Network.MoHWS.Logger (
    Handle,
    start,
    stop,
    log,
   ) where

import Network.MoHWS.Utility (dirname, )

import qualified Control.Exception as Exception
import Control.Exception (SomeException(SomeException), )
import Control.Concurrent (Chan, ThreadId, newChan, forkIO, writeChan, readChan, )
import System.Directory (createDirectoryIfMissing, )
import System.IO (IOMode(AppendMode), hPutStrLn, stderr, hClose, hFlush, )
import qualified System.IO as IO

import Prelude hiding (log, )


data Handle a = Handle
    {
     handleChan     :: Chan (Command a),
     handleThreadId :: ThreadId
    }

data T a = Cons
    {
     chan     :: Chan (Command a),
     format   :: (a -> IO String),
     file     :: FilePath
    }

data Command a = Stop | Log a

start ::
      (a -> IO String) -- ^ Message formatting function
   -> FilePath         -- ^ log file path
   -> IO (Handle a)
start format0 file0 =
    do chan0 <- newChan
       createDirectoryIfMissing True (dirname file0)
       let l = Cons {
                chan = chan0,
                format = format0,
                file = file0
               }
       t <- forkIO $
          run l
          `Exception.catch`
          \(SomeException e) ->
              hPutStrLn stderr
                 ("Error starting logger: " ++ show e)
       return $
          Handle {
             handleChan = chan0,
             handleThreadId = t
          }

stop :: Handle a -> IO ()
stop l = writeChan (handleChan l) Stop

log :: Handle a -> a -> IO ()
log l x = writeChan (handleChan l) (Log x)

-- Internals

run :: T a -> IO ()
run l =
   run1 l
   `Exception.catch`
   \(SomeException e) ->
      do hPutStrLn stderr ("Logger died: " ++ show e)
         run l

run1 :: T a -> IO ()
run1 l =
    Exception.bracket
      (openFile (file l))
      (\hdl -> hClose hdl)
      (\hdl -> handleCommands l hdl)
  where
    openFile :: FilePath -> IO IO.Handle
    openFile f =
        IO.openFile f AppendMode
        `Exception.catch`
        \(SomeException e) ->
           do hPutStrLn stderr ("Failed to open log file: " ++ show e)
              Exception.throw e

handleCommands :: T a -> IO.Handle -> IO ()
handleCommands l hdl =
    do comm <- readChan (chan l)
       case comm of
         Stop -> return ()
         Log x ->
            do writeLine hdl =<< format l x
               handleCommands l hdl
  where
    writeLine :: IO.Handle -> String -> IO ()
    writeLine hndl str =
       do hPutStrLn hndl str
          hFlush hndl
