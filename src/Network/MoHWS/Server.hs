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
{-# LANGUAGE Rank2Types #-}
module Network.MoHWS.Server (main, mainWithOptions, ) where

import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Environment as ServerEnv
import qualified Network.MoHWS.Server.Context as ServerContext
import Network.MoHWS.Logger.Error (debug, logError, logInfo, )
import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Logger.Access as AccessLogger
import qualified Network.MoHWS.Logger.Error as ErrorLogger
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Initialization as Init
import qualified Network.MoHWS.HTTP.MimeType as MimeType
import qualified Network.MoHWS.Server.Options as Options
import Network.MoHWS.ParserUtility (getUntilEmptyLine, )
import qualified Network.MoHWS.HTTP.Version as Version
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Request  as Request
import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Stream as Stream
import qualified Network.MoHWS.Utility as Util

import Data.Monoid (mempty, )
import Data.Maybe (catMaybes, )
import Data.Tuple.HT (swap, )
import Data.List.HT (viewR, )
import qualified Data.Set as Set

import qualified Control.Monad.Exception.Synchronous as Exc
import qualified Control.Exception as Exception
import Control.Monad.Exception.Synchronous (ExceptionalT, runExceptionalT, )
import Control.Monad.Trans.State (StateT, runStateT, modify, )
import Control.Monad.Trans.Class (lift, )

import qualified Network.Socket as Socket
import qualified Network.BSD as BSD
import Control.Concurrent (myThreadId, ThreadId, throwTo, killThread, forkIO, )
import Control.Exception (ErrorCall(ErrorCall), finally, mask, )
import Control.Monad (liftM, when, )
import Network.BSD (HostEntry, hostName, )
import Network.Socket (Socket, HostAddress, Family(AF_INET), )
import Network.URI (uriPath, )

import qualified System.Posix as Posix
import qualified System.IO as IO
import System.IO.Error (isAlreadyInUseError, isEOFError, catchIOError, )
import System.Environment (getArgs, )
import System.Posix (installHandler, sigHUP, sigPIPE, )
import Text.ParserCombinators.Parsec (parse, choice, )


{- -----------------------------------------------------------------------------
ToDo:

- MAJOR:

- deal with http version numbers
- timeouts (partly done)
- languages
- per-directory permissions (ala apache)
- error logging levels
- per-directory config options.
- languages (content-language, accept-language)
- multipart/byteranges

- MINOR:

- access logging (various bits left)
- implement user & group setting
- log time to serve request
- terminate & restart signal (like Apache's SIGHUP)
- don't die if the new configuration file contains errors after a restart
- reading config file may block, unsafe if we receive another SIGHUP
- common up headers with same name (eg. accept).
- implement if-modified-since (need to parse time)

- MAYBE:

- throttling if too many open connections (config: MaxClients)

-}


-----------------------------------------------------------------------------
-- Top-level server

main :: (Stream.C body) =>
   Init.T body ext -> IO ()
main initExt =
    do args <- getArgs
       case Options.parse args of
         Left err   -> Util.die err
         Right opts -> mainWithOptions initExt opts

mainWithOptions :: (Stream.C body) =>
   Init.T body ext -> Options.T -> IO ()
mainWithOptions initExt opts =
    do main_thread <- myThreadId
       _ <- installHandler sigPIPE Posix.Ignore Nothing
       _ <- installHandler sigHUP (Posix.Catch (hupHandler main_thread)) Nothing
       mask (readConfig initExt opts)

type Unblock a = IO a -> IO a

hupHandler :: ThreadId -> IO ()
hupHandler main_thread =
   throwTo main_thread (ErrorCall "**restart**")

sigsToBlock :: Posix.SignalSet
sigsToBlock = Posix.addSignal sigHUP Posix.emptySignalSet

-- Async exceptions should be blocked on entry to readConfig (so that
-- multiple SIGHUPs close together can't kill us).  Make sure that
-- there aren't any interruptible operations until we've blocked signals.
readConfig :: (Stream.C body) =>
   Init.T body ext -> Options.T -> (forall a. Unblock a) -> IO ()
readConfig initExt opts unblock = do
    Posix.blockSignals sigsToBlock
    r <- ConfigParser.run
            (choice $ map ModuleDesc.configParser $ Init.moduleList initExt)
            (Options.configPath opts)
    case r of
      Left err ->
         Util.die $ unlines $
         "Failed to parse configuration file" : show err : []
      Right b  -> do
        let updates = map ModuleDesc.setDefltConfig $ Init.moduleList initExt
            confExtDeflt =
               foldl (flip ($)) (Init.configurationExtensionDefault initExt) updates
            conf = b (Config.deflt confExtDeflt)
        st <- initServerState opts conf
        mods <- fmap catMaybes $ mapM (loadModule st) $ Init.moduleList initExt
        topServer st mods initExt unblock

rereadConfig :: (Stream.C body) =>
   ServerContext.T ext -> Init.T body ext -> (forall a. Unblock a) -> IO ()
rereadConfig st initExt unblock =
    do mapM_ AccessLogger.stop (ServerContext.accessLoggers st)
       ErrorLogger.stop (ServerContext.errorLogger st)
       readConfig initExt (ServerContext.options st) unblock


initServerState :: Options.T -> Config.T ext -> IO (ServerContext.T ext)
initServerState opts conf =
    do host <- do ent <- BSD.getHostEntry
                  case serverName conf of
                    "" -> return ent
                    n  -> return ent { hostName = n }
       mimeTypes
           <- MimeType.loadDictionary (Options.inServerRoot opts (typesConfig conf))
       errorLogger
           <- ErrorLogger.start (Options.inServerRoot opts (errorLogFile conf)) (logLevel conf)
       accessLoggers
          <- sequence [AccessLogger.start format (Options.inServerRoot opts file)
                       | (file,format) <- customLogs conf]

       let st = ServerContext.Cons
                {
                 ServerContext.options = opts,
                 ServerContext.config = conf,
                 ServerContext.hostName = host,
                 ServerContext.mimeTypes = mimeTypes,
                 ServerContext.errorLogger = errorLogger,
                 ServerContext.accessLoggers = accessLoggers
                }

       return st

loadModule :: (Stream.C body) =>
   ServerContext.T ext -> ModuleDesc.T body ext -> IO (Maybe (Module.T body))
loadModule st md =
    (do logInfo st $ "Loading module " ++ ModuleDesc.name md ++ "..."
        fmap Just $ ModuleDesc.load md st)
    `Exception.catch`
    \(Exception.SomeException e) ->
          do logError st $ unlines ["Error loading module " ++ ModuleDesc.name md,
                                    show e]
             return Nothing

-- We catch exceptions from the main server thread, and restart the
-- server.  If we receive a restart signal (from a SIGHUP), then we
-- re-read the configuration file.
topServer :: (Stream.C body) =>
   ServerContext.T ext -> [Module.T body] -> Init.T body ext -> (forall a. Unblock a) -> IO ()
topServer st mods initExt unblock =
   let startServers =
          do ts <- servers st mods
             (Util.wait `Exception.catch`
              (\e -> case e of
                       ErrorCall "**restart**" ->
                           do mapM_ killThread ts
                              rereadConfig st initExt unblock
                       _ -> Exception.throw e))
       loop =
          (do Posix.unblockSignals sigsToBlock
              unblock startServers)
          `Exception.catch`
          (\(Exception.SomeException e) ->
                 do logError st ("server: " ++ show e)
                    loop)
   in  loop

servers :: (Stream.C body) =>
   ServerContext.T ext -> [Module.T body] -> IO [ThreadId]
servers st mods =
   let mkEnv port =
          ServerEnv.Cons {
             ServerEnv.context = st,
             ServerEnv.modules = mods,
             ServerEnv.port    = port
          }

       mkAddr (maddr,port) =
          do addr <- case maddr of
                       Nothing -> return Socket.iNADDR_ANY
                       Just ip -> Socket.inet_addr ip
             return (mkEnv port, Socket.SockAddrInet port addr)

   in  do addrs <- mapM mkAddr (listen (ServerContext.config st))
          mapM (\ (env,addr) -> forkIO (server env addr)) addrs


-- open the server socket and start accepting connections
server :: (Stream.C body) =>
   ServerEnv.T body ext -> Socket.SockAddr -> IO ()
server st addr = do
  logInfo st $ "Starting server thread on " ++ show addr
  proto <- BSD.getProtocolNumber "tcp"
  Exception.bracket
     (Socket.socket AF_INET Socket.Stream proto)
     (\sock -> Socket.sClose sock)
     (\sock -> do Socket.setSocketOption sock Socket.ReuseAddr 1
                  ok <- Util.catchSomeIOErrors isAlreadyInUseError
                        (Socket.bindSocket sock addr >> return True)
                        (\e -> do logError st ("server: " ++ show e)
                                  IO.hPutStrLn IO.stderr $ show e
                                  return False)
                  when ok $ do Socket.listen sock Socket.maxListenQueue
                               acceptConnections st sock)

-- accept connections, and fork off a new thread to handle each one
acceptConnections :: (Stream.C body) =>
   ServerEnv.T body ext -> Socket -> IO ()
acceptConnections st sock = do
  debug st "Calling accept..."
  (h, Socket.SockAddrInet port haddr) <- Util.accept sock
  Socket.inet_ntoa haddr >>=
                \ip -> debug st $ "Got connection from " ++ ip ++ ":" ++ show port
  _ <- forkIO (
          (talk st h haddr  `finally`  IO.hClose h)
            `Exception.catch`
          (\(Exception.SomeException e) ->
              debug st ("servlet died: "  ++ show e))
        )
  acceptConnections st sock

talk :: (Stream.C body) =>
   ServerEnv.T body ext -> IO.Handle -> HostAddress -> IO ()
talk st h haddr = do
  debug st "Started"
  IO.hSetBuffering h IO.LineBuffering
  run st True h haddr
  debug st "Done"

run :: (Stream.C body) =>
   ServerEnv.T body ext -> Bool -> IO.Handle -> HostAddress -> IO ()
run st first h haddr = do
    let conf = ServerEnv.config st
    -- read a request up to the first empty line.  If we
    -- don't get a request within the alloted time, issue
    -- a "Request Time-out" response and close the connection.
    let time_allowed =
           if first
             then requestTimeout conf
             else keepAliveTimeout conf

    debug st "Waiting for request..."
    req <- catchIOError (
             do ok <- IO.hWaitForInput h (time_allowed * 1000)
                if ok then liftM Just (getUntilEmptyLine h)
                  -- only send a "request timed out" response if this
                  -- was the first request on the socket.  Subsequent
                  -- requests time-out and close the socket silently.
                  -- ToDo: if we get a partial request, still emit the
                  -- the timeout response.
                      else do debug st $ "Request timeout (after " ++ show time_allowed ++ " s)"
                              when first (response st h (Response.makeRequestTimeOut conf))
                              return Nothing
                              )
           (\e ->
                if isEOFError e
                     then debug st "EOF from client" >> return Nothing
                     else do logError st ("request: " ++ show e)
                             return Nothing )

    case req of { Nothing -> return ();  Just r -> do
    case parse Request.pHeaders "Request" r of

         -- close the connection after a badly formatted request
         Left err -> do
              debug st (show err)
              response st h (Response.makeBadRequest conf)
              return ()

         Right req_no_body  -> do
              reqt <- getBody h req_no_body
              debug st $ show reqt
              resp <- request st reqt haddr
              response st h resp

              -- Persistent Connections
              --
              -- We close the connection if
              --   (a) client specified "connection: close"
              --   (b) client is pre-HTTP/1.1, and didn't
              --       specify "connection: keep-alive"

              let connection_headers = Request.getConnection (Request.headers reqt)
              if Request.ConnectionClose `elem` connection_headers
                 || (Request.httpVersion reqt < Version.http1_1
                     && Request.ConnectionKeepAlive `notElem` connection_headers)
                   then return ()
                   else run st False h haddr
   }


getBody :: (Stream.C body) =>
   IO.Handle -> Request.T body -> IO (Request.T body)
getBody h req =
   let -- FIXME: handled chunked input
       readBody =
          case Header.getContentLength req of
             Nothing  -> return mempty
             -- FIXME: what if input is huge?
             Just len -> Stream.read h len
   in  do b <- readBody
          return $ req { Request.body = b}

-----------------------------------------------------------------------------
-- Dealing with requests

request :: (Stream.C body) =>
   ServerEnv.T body ext -> Request.T body -> HostAddress -> IO (Response.T body)
request st req haddr =
    do (sreq,merr) <- serverRequest st req haddr
       resp <- case merr of
                 Nothing  -> do sreq' <- tweakRequest st sreq
                                debug st $ "Handling request..."
                                handleRequest st sreq'
                 Just err -> return err
       debug st (Response.showStatusLine resp)
       ServerEnv.logAccess st sreq resp (error "noTimeDiff"){-FIXME-}
       return resp

serverRequest :: (Stream.C body) =>
   ServerEnv.T body ext -> Request.T body -> HostAddress -> IO (ServerRequest.T body, Maybe (Response.T body))
serverRequest st req haddr =
   let conf = ServerEnv.config st
       sreq =
          ServerRequest.Cons {
             ServerRequest.clientRequest   = req,
             ServerRequest.clientAddress   = haddr,
             ServerRequest.clientName      = Nothing,
             ServerRequest.requestHostName = ServerEnv.hostName st,
             ServerRequest.serverURIPath   = "-",
             ServerRequest.serverFilename  = "-",
             ServerRequest.serverPort      = ServerEnv.port st
          }
       maybeExc x =
          case x of
             Exc.Success   _ -> Nothing
             Exc.Exception e -> Just e
   in  fmap swap (runStateT
          (fmap maybeExc $ runExceptionalT $ serverRequestExc st req haddr) sreq)
       `Exception.catch`
       ( \(Exception.SomeException exception) -> do
            logError st ("request: " ++ show exception)
            return (sreq, Just (Response.makeInternalServerError conf))
       )

serverRequestExc :: (Stream.C body) =>
   ServerEnv.T body ext -> Request.T body -> HostAddress -> ExceptionalT (Response.T body) (StateT (ServerRequest.T body) IO) ()
serverRequestExc st req haddr =
   let conf = ServerEnv.config st
       use = Exc.mapExceptionalT lift
       update = lift . modify
   in  do remoteName <- use $ lift $ maybeLookupHostname conf haddr
          update $ \sreq -> sreq { ServerRequest.clientName = remoteName }
          host <- use $ getServerHostName st req
          update $ \sreq -> sreq { ServerRequest.requestHostName = host }
          path <- use $ requestAbsPath st req
          update $ \sreq -> sreq { ServerRequest.serverURIPath = path }
          file <- use $ translatePath st (hostName host) path
          update $ \sreq -> sreq { ServerRequest.serverFilename = file }



maybeLookupHostname :: Config.T ext -> HostAddress -> IO (Maybe HostEntry)
maybeLookupHostname conf haddr =
    if hostnameLookups conf
      then catchIOError
              (liftM Just (BSD.getHostByAddr AF_INET haddr))
              (\_ -> return Nothing)
      else return Nothing

type EIO body = ExceptionalT (Response.T body) IO

-- make sure we've got a host field
-- if the request version is >= HTTP/1.1
getServerHostName :: (Stream.C body) =>
   ServerEnv.T body ext -> Request.T body -> EIO body HostEntry
getServerHostName st req =
   let conf = ServerEnv.config st
       isServerHost host =
          host `Set.member` (Set.insert (serverName conf) $ serverAlias conf) ||
          any (flip Module.isServerHost host) (ServerEnv.modules st)
   in  case Request.getHost req of
          Nothing ->
             if Request.httpVersion req < Version.http1_1
               then return $ ServerEnv.hostName st
               else Exc.throwT $ Response.makeBadRequest conf
          Just (host,_) ->
             if isServerHost host
               then return $ (ServerEnv.hostName st) { hostName = host }
               else do lift $ logError st ("Unknown host: " ++ show host)
                       Exc.throwT $ Response.makeNotFound conf


-- | Get the absolute path from the request.
requestAbsPath :: (Stream.C body) =>
   ServerEnv.T body ext -> Request.T body -> EIO body String
requestAbsPath _ req = return $ uriPath $ Request.uri req


-- Path translation

translatePath :: (Stream.C body) =>
   ServerEnv.T body ext -> String -> String -> EIO body FilePath
translatePath st host pth =
  do m_file <- lift $ ServerEnv.tryModules st (\m -> Module.translatePath m host pth)
     case m_file of
       Just file -> return $ file
       Nothing   -> defaultTranslatePath st pth

defaultTranslatePath :: (Stream.C body) =>
   ServerEnv.T body ext -> String -> EIO body FilePath
defaultTranslatePath st pth =
   let conf = ServerEnv.config st
   in  case pth of
         '/':_ -> return $ documentRoot conf ++ pth
         _     -> Exc.throwT $ Response.makeNotFound conf

-- Request tweaking

tweakRequest :: (Stream.C body) =>
   ServerEnv.T body ext -> ServerRequest.T body -> IO (ServerRequest.T body)
tweakRequest st =
   ServerEnv.foldModules st (\m r -> Module.tweakRequest m r)

-- Request handling

handleRequest :: (Stream.C body) =>
   ServerEnv.T body ext -> ServerRequest.T body -> IO (Response.T body)
handleRequest st req =
    do m_resp <- ServerEnv.tryModules st (\m -> Module.handleRequest m req)
       case m_resp of
         Just resp -> return resp
         Nothing   -> defaultHandleRequest st req

defaultHandleRequest :: (Stream.C body) =>
   ServerEnv.T body ext -> ServerRequest.T body -> IO (Response.T body)
defaultHandleRequest st _ =
   return $ Response.makeNotFound $ ServerEnv.config st

-- Sending response


response :: (Stream.C body) =>
   ServerEnv.T body ext ->
   IO.Handle ->
   Response.T body ->
   IO ()

response env h
   (Response.Cons {
      Response.code        = code,
      Response.description = desc,
      Response.headers     = headers,
      Response.coding      = tes,
      Response.body        = body,
      Response.doSendBody  = sendBody
   }) =
  do
  Util.hPutStrCrLf h (Response.statusLine code desc)
  hPutHeader h Response.serverHeader

  -- Date Header: required on all messages
  date <- Response.dateHeader
  hPutHeader h date

  mapM_ (hPutHeader h) (Header.list headers)

  -- Output a Content-Length when the message body isn't
  -- encoded.  If it *is* encoded, then the last transfer
  -- coding must be "chunked", according to RFC2616 sec 3.6.  This
  -- allows the client to determine the message-length.
  let contentLength = Response.size body

  when (Response.hasBody body && null tes)
     (maybe (return ()) (hPutHeader h . Header.makeContentLength) contentLength)

  mapM_ (hPutHeader h . Header.makeTransferCoding) tes

  Util.hPutStrCrLf h ""
  -- ToDo: implement transfer codings

  let conf = ServerEnv.config env

  when sendBody $
     case viewR tes of
        Just (_, Header.ChunkedTransferCoding) ->
             Response.sendBodyChunked (Config.chunkSize conf) h body
        _ -> Response.sendBody h body

hPutHeader :: IO.Handle -> Header.T -> IO ()
hPutHeader h =
   IO.hPutStr h . show
--   Util.hPutStrCrLf h . show
