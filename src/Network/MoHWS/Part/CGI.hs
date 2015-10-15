{- |
Copyright: 2006, Bjorn Bringert.
Copyright: 2009, Henning Thielemann.
-}
module Network.MoHWS.Part.CGI (
   Configuration, desc,
   mkCGIEnv, mkCGIResponse,
   ) where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Request as Request
import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Stream as Stream
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Context as ServerContext
import Network.MoHWS.Logger.Error (debug, abort, debugOnAbort, logError, )
import qualified Network.MoHWS.Utility as Util

import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Configuration.Accessor as ConfigA
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.Basic ((.>))
import qualified Text.ParserCombinators.Parsec as Parsec
import Network.MoHWS.ParserUtility (trimLWS, )

import Data.Maybe.HT (toMaybe, )
import Data.Tuple.HT (mapFst, )
import Data.Bool.HT (if', )
import Control.Monad.Trans.Maybe (MaybeT, )
import Control.Concurrent (forkIO, )
import qualified Control.Exception as Exception
import Control.Monad.Trans.Class (lift, )
import Control.Monad (when, mzero, )
import Data.Char (toUpper, )
import Data.List (isSuffixOf, )
import Network.BSD (hostName, )
import Network.Socket (inet_ntoa, )
import Network.URI (uriQuery, )
import qualified System.IO as IO
import System.IO.Error (isEOFError, )
import System.Posix (isDirectory, isRegularFile, isSymbolicLink, )
import System.Process (runInteractiveProcess, waitForProcess, )
import Text.ParserCombinators.Parsec (parse, )


desc :: (Stream.C body) => ModuleDesc.T body Configuration
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "cgi",
      ModuleDesc.load = return . funs,
      ModuleDesc.configParser = parser,
      ModuleDesc.setDefltConfig = const defltConfig
   }

data Configuration =
   Configuration {
      suffixes_ :: [String]
   }

defltConfig :: Configuration
defltConfig =
   Configuration {
      suffixes_ = [".cgi"]
   }

suffixes :: Accessor.T Configuration [String]
suffixes =
   Accessor.fromSetGet (\x c -> c{suffixes_ = x}) suffixes_

parser :: ConfigParser.T st Configuration
parser =
   ConfigParser.field "cgisuffixes" p_suffixes

p_suffixes :: ConfigParser.T st Configuration
p_suffixes =
   ConfigParser.set (ConfigA.extension .> suffixes) $
   Parsec.many ConfigParser.stringLiteral

funs :: (Stream.C body) =>
   ServerContext.T Configuration -> Module.T body
funs st =
   Module.empty {
      Module.handleRequest = handleRequest st
   }

handleRequest :: (Stream.C body) =>
   ServerContext.T Configuration -> ServerRequest.T body -> MaybeT IO (Response.T body)
handleRequest st sreq =
    do let conf = ServerContext.config st
       (pathProg, pathInfo) <-
          debugOnAbort st ("CGI: not handling " ++ ServerRequest.serverFilename sreq) $
          findProg st (ServerRequest.serverFilename sreq)
       let sufs = suffixes_ $ Config.extension conf
       when (not $ any (flip isSuffixOf pathProg) sufs)
          (abort st $ "CGI: not handling " ++ ServerRequest.serverFilename sreq ++ ", wrong suffix")
       let hndle = handleRequest2 st sreq pathProg pathInfo
       lift $
          case Request.command (ServerRequest.clientRequest sreq) of
             Request.GET  -> hndle False
             Request.POST -> hndle True
             _ -> return $ Response.makeNotImplemented conf

handleRequest2 :: (Stream.C body) =>
   ServerContext.T ext -> ServerRequest.T body -> FilePath -> String -> Bool -> IO (Response.T body)
handleRequest2 st sreq pathProg pathInfo useReqBody =
    do let conf = ServerContext.config st
       let req = ServerRequest.clientRequest sreq

       env <- mkCGIEnv st sreq pathInfo
       let wdir = Util.dirname pathProg
           prog = "./" ++ Util.basename pathProg

       debug st $ "Running CGI program: " ++ prog ++ " in " ++ wdir

       (inp,out,err,pid)
           <- runInteractiveProcess prog [] (Just wdir) (Just env)


       if useReqBody
         then forkIO (writeBody inp req) >> return ()
         else IO.hClose inp

       -- log process stderr to the error log
       _ <- forkIO (logErrorsFromHandle st err)

       -- FIXME: exception handling
       -- FIXME: close handle?
       output <- Stream.readAll (Config.chunkSize conf) out

       -- wait in a separate thread, so that this thread can continue.
       -- this is needed since output is lazy.
       _ <- forkIO (waitForProcess pid >> return ())

       case parseCGIOutput output of
         Left errp ->
            do logError st errp
               return $ Response.makeInternalServerError conf
         Right (outputHeaders, content) ->
            mkCGIResponse outputHeaders content out

mkCGIResponse :: Header.Group -> body -> IO.Handle -> IO (Response.T body)
mkCGIResponse outputHeaders content h =
    do let stat = Header.lookup (Header.HdrCustom "Status") outputHeaders
           loc  = Header.lookup Header.HdrLocation outputHeaders
       (code,dsc) <-
          case stat of
             Nothing -> let c = maybe 200 (\_ -> 302) loc
                        in  return (c, Response.descriptionFromCode c)
             Just s  -> case reads s of
                          [(c,r)] -> return (c, trimLWS r)
                          _       -> fail "Bad Status line"

       let body =
              Response.Body {
                 Response.size = Nothing,
                 Response.source = "CGI script",
                 Response.close = IO.hClose h,
                 Response.content = content
              }

       -- FIXME: don't use response constructor directly
       return $
          Response.Cons code dsc outputHeaders [Header.ChunkedTransferCoding] True body

-- Split the requested file system path into the path to an
-- existing file, and some extra path info
findProg :: ServerContext.T ext -> FilePath -> MaybeT IO (FilePath,String)
findProg st filename =
   case Util.splitPath filename of
      []    -> mzero -- this should never happen
      [""]  -> mzero -- we got an empty path
      "":p  -> firstFile st "/" p -- absolute path
      p:r   -> firstFile st p r -- relative path

-- similar to Module.File.handleRequest
firstFile :: ServerContext.T ext -> FilePath -> [String] -> MaybeT IO (FilePath,String)
firstFile st p pis =
   let conf = ServerContext.config st

       mkPath x y =
          if Util.hasTrailingSlash x
            then x ++ y
            else x ++ "/" ++ y

       mkPathInfo [] = ""
       mkPathInfo q  = "/" ++ Util.glue "/" q

       checkStat stat =
          if' (isDirectory stat)
             (case pis of
                []     -> abort st $ "findProg: " ++ show p ++ " is a directory"
                f:pis' -> firstFile st (mkPath p f) pis') $
          if' (isRegularFile stat) (return (p,mkPathInfo pis)) $
          if' (isSymbolicLink stat)
             (if Config.followSymbolicLinks conf
                then Util.statFile p >>= checkStat
                else abort st ("findProg: Not following symlink: " ++ show p)) $
          (abort st $ "Strange file: " ++ show p)
   in  debugOnAbort st ("findProg: Not found: " ++ show p) (Util.statSymLink p) >>=
       checkStat

mkCGIEnv :: ServerContext.T ext -> ServerRequest.T body -> String -> IO [(String,String)]
mkCGIEnv _st sreq pathInfo =
      do let req = ServerRequest.clientRequest sreq
         remoteAddr <- inet_ntoa (ServerRequest.clientAddress sreq)
         let scriptName = ServerRequest.serverURIPath sreq `Util.dropSuffix` pathInfo
             -- FIXME: use canonical name if there is no ServerName
             serverEnv =
                 [
                  ("SERVER_SOFTWARE",   Config.serverSoftware
                                        ++ "/" ++ Config.serverVersion),
                  ("SERVER_NAME",       hostName (ServerRequest.requestHostName sreq)),
                  ("GATEWAY_INTERFACE", "CGI/1.1")
                 ]
             requestEnv =
                 [
                  ("SERVER_PROTOCOL",   show (Request.httpVersion req)),
                  ("SERVER_PORT",       show (ServerRequest.serverPort sreq)),
                  ("REQUEST_METHOD",    show (Request.command req)),
                  ("PATH_TRANSLATED",   ServerRequest.serverFilename sreq),
                  ("SCRIPT_NAME",       scriptName),
                  ("QUERY_STRING",      uriQuery (Request.uri req) `Util.dropPrefix` "?"),
                  ("REMOTE_ADDR",       remoteAddr),
                  ("PATH_INFO",         pathInfo),
                  ("PATH",              "/usr/local/bin:/usr/bin:/bin")
                 ]
               ++ maybeHeader "AUTH_TYPE"      Nothing -- FIXME
               ++ maybeHeader "REMOTE_USER"    Nothing -- FIXME
               ++ maybeHeader "REMOTE_IDENT"   Nothing -- FIXME
               ++ maybeHeader "REMOTE_HOST"    (fmap hostName (ServerRequest.clientName sreq))
               ++ maybeHeader "CONTENT_TYPE"   (Header.getContentType req)
               ++ maybeHeader "CONTENT_LENGTH" (fmap show $ Header.getContentLength req)
             hs = [] -- FIXME: convert headers to (name,value) pairs
             headerEnv = [("HTTP_"++ map toUpper n, v) | (n,v) <- hs]

         return $ serverEnv ++ requestEnv ++ headerEnv

-- Writes the body of a request to a handle.
writeBody :: (Stream.C body) =>
   IO.Handle -> Request.T body -> IO ()
writeBody h req =
   Stream.write h (Request.body req)
   `Exception.finally`
   IO.hClose h

-- | Reads lines form the given 'Handle' and log them with 'logError'.
logErrorsFromHandle :: ServerContext.T ext -> IO.Handle -> IO ()
logErrorsFromHandle st h =
    (Exception.catchJust (\ e -> toMaybe (isEOFError e) e)
        loop (const $ return ())
     `Exception.catch`
     \(Exception.SomeException e) -> logError st $ "CGI:" ++ show e)
      `Exception.finally` IO.hClose h
  where loop = do l <- IO.hGetLine h
                  logError st l
                  loop

maybeHeader :: String -> Maybe String -> [(String,String)]
maybeHeader n = maybe [] ((:[]) . (,) n)

{-
expects CRLF line endings, which is too strict

parseCGIOutput :: B.ByteString -> Either String (Header.Group, B.ByteString)
parseCGIOutput s =
   let (hdrsStr, body) = breakHeaders s
   in  case parse Header.pGroup "CGI output" hdrsStr of
          Left err -> Left (show err)
          Right hdrs  -> Right (hdrs, body)

breakHeaders :: B.ByteString -> (String, B.ByteString)
breakHeaders =
   (\(hdrs, body) ->
      mapFst (map B.head hdrs ++) $
      if B.null $ head body
        then ("", B.empty)
        else (crLf, body!!4)) .
   break (\suffix -> B.isPrefixOf (B.pack (crLf++crLf)) suffix || B.null suffix) .
   B.tails
-}

parseCGIOutput :: (Stream.C body) => body -> Either String (Header.Group, body)
parseCGIOutput s =
   let (hdrLines, body) = breakHeaders s
   in  -- parse headers in one go in order to handle multi-line headers correctly
       case parse Header.pGroup "CGI output" $ unlines hdrLines of
          Left err -> Left (show err)
          Right hdrs -> Right (hdrs, body)

breakHeaders :: (Stream.C body) => body -> ([String], body)
breakHeaders str =
   let (hdr,rest0) = Stream.break (\c -> c=='\r' || c=='\n') str
       skip =
          if Stream.isPrefixOf (Stream.fromString 2 "\r\n") rest0 ||
             Stream.isPrefixOf (Stream.fromString 2 "\n\r") rest0
            then 2 else 1
       rest1 = Stream.drop skip rest0
   in  if Stream.isEmpty hdr
         then ([], rest1)
         else mapFst (Stream.toString hdr :) $ breakHeaders rest1
