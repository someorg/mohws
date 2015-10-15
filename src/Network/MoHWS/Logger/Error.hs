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

module Network.MoHWS.Logger.Error (
    Handle,
    start,
    stop,
    log,

    HasHandle(getHandle),
    debug,
    abort,
    debugOnAbort,
    logError,
    logInfo,
    logDebug,
   ) where

import qualified Network.MoHWS.Logger as Logger
import qualified Network.MoHWS.Logger.Level as LogLevel
import Network.MoHWS.Utility (formatTimeSensibly, )

import System.Time (ClockTime, toUTCTime, getClockTime, )

import Control.Concurrent (myThreadId, )
import Control.Monad.IO.Class (MonadIO, liftIO, )
import Control.Monad.Trans.Class (lift, )
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT, )
import Control.Monad (mzero, )


import Prelude hiding (log, )


data Handle = Handle
    {
     logger ::Logger.Handle Message,
     minLevel :: LogLevel.T
    }

data Message = Message
    {
     time   :: ClockTime,
     string :: String
    }


start :: FilePath -> LogLevel.T -> IO Handle
start file level =
    do l <- Logger.start (return . format) file
       let h = Handle {
                logger = l,
                minLevel = level
               }
       log h LogLevel.Warn $ "Starting error logger with log level "
                                    ++ show level ++ "..."
       return h
  where format m = formatTimeSensibly (toUTCTime (time m))
                   ++ "  " ++ string m

stop :: Handle -> IO ()
stop l =
   do log l LogLevel.Warn "Stopping error logger..."
      Logger.stop (logger l)

log :: Handle -> LogLevel.T -> String -> IO ()
log l level s =
   if level < minLevel l
     then return ()
     else do t <- getClockTime
             Logger.log (logger l) (Message t s)


-- * logging in more general contexts

class HasHandle h where
   getHandle :: h -> Handle

instance HasHandle Handle where
   getHandle = id


debug :: (HasHandle h, MonadIO io) => h -> String -> io ()
debug h s =
   liftIO $
   do t <- myThreadId
      logDebug h $ show t ++ ": " ++ s

abort :: (HasHandle h) => h -> String -> MaybeT IO a
abort h s = lift (debug h s) >> mzero

debugOnAbort :: (HasHandle h) => h -> String -> MaybeT IO a -> MaybeT IO a
debugOnAbort h s act =
   MaybeT $
   do x <- runMaybeT act
      case x of
         Nothing -> debug h s
         _ -> return ()
      return x

logError :: (HasHandle h) => h -> String -> IO ()
logError h = log (getHandle h) LogLevel.Error

logInfo :: (HasHandle h) => h -> String -> IO ()
logInfo h = log (getHandle h) LogLevel.Info

logDebug :: (HasHandle h) => h -> String -> IO ()
logDebug h = log (getHandle h) LogLevel.Debug
