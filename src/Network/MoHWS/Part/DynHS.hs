-- Copyright 2006 Bjorn Bringert
module Network.MoHWS.Part.DynHS where

import Network.MoHWS.Part.DynHS.GHCUtil
          (Session, initGHC, setLogAction, withCleanUp, getFileValue, )

import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Module             as Module
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Context as ServerContext
import qualified Network.MoHWS.Stream as Stream
import Network.MoHWS.Server.Request (serverFilename, )
import Network.MoHWS.Logger.Error (debug, logError, )

import Network.MoHWS.Configuration as Config

import Control.Exception as Exception
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), )
import Control.Monad (liftM, mzero, )
import Data.List (isSuffixOf, )
import Data.Tuple.HT (mapFst, )


-- FIXME: keep this in config
packageDirectory :: FilePath
packageDirectory = "/usr/lib/ghc-6.8.2"  -- last working version was 6.6


desc :: (Stream.C body) => ModuleDesc.T body ext
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "dynhs",
      ModuleDesc.load = loadDynHS
   }

loadDynHS :: (Stream.C body) =>
   ServerContext.T ext -> IO (Module.T body)
loadDynHS st =
    do s <- initGHC packageDirectory
       dynhsLoadConfig s st
       return $
          Module.empty {
             Module.handleRequest = dynhsHandleRequest s st
          }

dynhsLoadConfig :: Session -> ServerContext.T ext -> IO ()
dynhsLoadConfig s st = setLogAction s (logError st)

dynhsHandleRequest :: (Stream.C body) =>
   Session -> ServerContext.T ext -> ServerRequest.T body -> MaybeT IO (Response.T body)
dynhsHandleRequest s st sreq =
    if ".hs" `isSuffixOf` serverFilename sreq
      then dynhsHandleRequest2 s st sreq
      else mzero

dynhsHandleRequest2 :: (Stream.C body) =>
   Session -> ServerContext.T ext -> ServerRequest.T body -> MaybeT IO (Response.T body)
dynhsHandleRequest2 s st sreq =
   MaybeT $ withCleanUp s $
-- FIXME: lots of fake stuff here
    do debug st $ "DynHS: Loading " ++ show (serverFilename sreq)
       e_cgiMain <- logGHCErrors s st (getCgiMain s (serverFilename sreq))
       case e_cgiMain of
         Left resp -> return $ Just resp
         Right cgiMain ->
            do debug st $ "DynHS: Loaded successfully: " ++ show (serverFilename sreq)
               liftM Just $ runCgiMain st sreq cgiMain

type CGIMain = [(String,String)] -> [(String,String)] -> IO ([(String,String)], String)

getCgiMain :: Session -> FilePath -> IO CGIMain
getCgiMain s file = getFileValue s file "cgiMain"

runCgiMain :: (Stream.C body) =>
   ServerContext.T ext -> ServerRequest.T body -> CGIMain -> IO (Response.T body)
runCgiMain st _sreq cgiMain =
    -- FIXME: lots of fake stuff here
    do let env = []
           inputs = []
       (hs,content) <- cgiMain env inputs
       let headers =
              Header.group $
              map (uncurry Header.make . mapFst Header.makeName) hs
       let code = 200
           descr = "OK"
       return $
          Response.Cons code descr headers [] True $
          Response.bodyWithSizeFromString $
          Stream.fromString (Config.chunkSize (ServerContext.config st)) content

-- GHC utilities

logGHCErrors :: (Stream.C body) =>
   Session -> ServerContext.T ext -> IO a -> IO (Either (Response.T body) a)
logGHCErrors _s st f =
    liftM Right f
    `Exception.catch`
    (\e -> do logError st (show e)
              -- FIXME: include error message in response
              return $ Left $ Response.makeInternalServerError (ServerContext.config st))

