module Network.MoHWS.Part.DynHS.CGI where

import Network.MoHWS.Part.CGI (mkCGIEnv, mkCGIResponse, )

import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.HTTP.Request  as Request
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Context as ServerContext
import ServerRequest (clientRequest, )

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Maybe (isJust)

import Network.CGI.Monad (CGI, runCGIT, )
import Network.CGI.Protocol


hwsRunCGI :: ServerContext.T ext -> ServerRequest.T -> CGI CGIResult -> IO (Response.T String)
hwsRunCGI st sreq cgi =
  do let path_info = "" -- FIXME: do the path walk
     env <- mkCGIEnv st sreq path_info
     let input = BS.pack $ Request.body $ clientRequest sreq
     (hs,body) <- runCGI_ env input (runCGIT cgi)
     mkCGIResponse hs (BS.unpack body)

-- | Run a CGI action. This is what runCGIEnvFPS really should look like.
runCGI_ :: Monad m =>
           [(String,String)] -- ^ CGI environment variables.
        -> ByteString -- ^ Request body.
        -> (CGIRequest -> m (Header.Group, CGIResult)) -- ^ CGI action.
        -> m (Header.Group, ByteString) -- ^ (Response.T String) (headers and content).
runCGI_ vars inp f
    = do (hs,outp) <- f $ CGIRequest {
                                      cgiVars = Map.fromList vars,
                                      cgiInputs = decodeInput vars inp,
                                      cgiRequestBody = inp
                                     }
         return $ case outp of
           CGIOutput c -> (hs',c)
               where hs' = if isJust (lookup ct hs)
                              then hs else hs ++ [(ct,defaultContentType)]
                     ct = HeaderName "Content-type"
           CGINothing -> (hs, BS.empty)

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"
