module Network.MoHWS.Part.DynHS.GHCUtil
    (Session,
     initGHC,
     setLogAction,
     withCleanUp,
     getFileValue,
     -- * Error logging
     Severity(..), SrcSpan, PprStyle, Message,
     mkLocMessage
    ) where

-- GHC API stuff
import DynFlags (initDynFlags, defaultDynFlags, )
import ErrUtils (Message, mkLocMessage, )
import GHC
import HscMain (newHscEnv, )
import HscTypes (Session(..), )
import Outputable (PprStyle, )
import SrcLoc (SrcSpan, )
import SysTools (initSysTools, )

import Data.Dynamic (Typeable, fromDynamic, )
import Data.IORef (newIORef, )


initGHC :: FilePath -> IO Session
initGHC pkgDir =
    do s <- newSession' CompManager (Just pkgDir)
       modifySessionDynFlags s (\dflags -> dflags{ hscTarget = HscInterpreted })
       return s



-- Like newSession, but does not install signal handlers
newSession' :: GhcMode -> Maybe FilePath -> IO Session
newSession' mode mb_top_dir = do
  dflags0 <- initSysTools mb_top_dir defaultDynFlags
  dflags  <- initDynFlags dflags0
  env <- newHscEnv dflags{ ghcMode=mode }
  ref <- newIORef env
  return (Session ref)

setLogAction :: Session -> (String -> IO ()) -> IO ()
setLogAction s f =
    modifySessionDynFlags s (\dflags -> dflags { log_action = mkLogAction f })

mkLogAction :: (String -> IO ())
            -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
mkLogAction f severity srcSpan style msg =
    case severity of
      SevInfo  -> f (show (msg style))
      SevFatal -> f (show (msg style))
      _        -> f (show ((mkLocMessage srcSpan msg) style))


modifySessionDynFlags :: Session -> (DynFlags -> DynFlags) -> IO ()
modifySessionDynFlags s f =
    do dflags <- getSessionDynFlags s
       setSessionDynFlags s (f dflags)
       return ()

withCleanUp :: Session -> IO a -> IO a
withCleanUp s f =
    do dflags <- getSessionDynFlags s
       defaultCleanupHandler dflags f

loadFile :: Session -> FilePath -> IO GHC.Module
loadFile s file =
    do let t = Target (TargetFile file Nothing) Nothing
       setTargets s [t]
       success <- load s LoadAllTargets
       case success of
         Succeeded -> do m <- fileModule s file
                         setContext s [] [m]
                         return m
         Failed    -> fail $ "Failed to load " ++ show file

fileModule :: Session -> FilePath -> IO GHC.Module
fileModule s f =
    do gr <- getModuleGraph s
       case [ms_mod ms | ms <- gr, ml_hs_file (ms_location ms) == Just f]  of
         [m] -> return m
         _   -> fail $ "File " ++ f ++ " does not correspond to a module"

getValue :: Typeable a => Session -> String -> IO a
getValue s x =
    do mdyn <- dynCompileExpr s x
       case mdyn of
         Nothing -> fail $ "dynCompileExpr " ++ show x ++ " failed"
         Just dyn -> case fromDynamic dyn of
                       Nothing -> fail $ "Type error: " ++ x
                                         ++ " is an " ++ show dyn
                       Just y  -> return y

getFileValue :: Typeable a => Session -> FilePath -> String -> IO a
getFileValue s file x =
    do loadFile s file
       getValue s x
