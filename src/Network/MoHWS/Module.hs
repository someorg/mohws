{- |
Copyright: 2006, Bjorn Bringert
Copyright: 2009, Henning Thielemann
-}
module Network.MoHWS.Module where

import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Server.Request as ServerRequest
import Control.Monad.Trans.Maybe (MaybeT, )
import Control.Monad (mzero, )
import Network.Socket (HostName, )


{- |
'isServerHost' allows for advanced checks of the appropriate domain,
e.g. we can catch all subdomains of a certain domain.
-}
data T body = Cons
   {
      isServerHost  :: HostName -> Bool,
      translatePath :: String -> String -> MaybeT IO FilePath,
      tweakRequest  :: ServerRequest.T body -> IO (ServerRequest.T body),
      handleRequest :: ServerRequest.T body -> MaybeT IO (Response.T body)
   }

empty :: T body
empty =
   Cons {
      isServerHost  = \_   -> False,
      translatePath = \_ _ -> mzero,
      tweakRequest  = \r   -> return r,
      handleRequest = \_   -> mzero
   }

{- |
We use the type variable 'server'
although it will be always instantiated with 'ServerContext.T'.
However, with this type variable we avoid mutual recursive Haskell modules
for Module and ServerContext.
-}
tweakFilename ::
   (server -> FilePath -> IO FilePath) ->
   server -> ServerRequest.T body -> IO (ServerRequest.T body)
tweakFilename f conf req =
    do filename <- f conf (ServerRequest.serverFilename req)
       return $ req { ServerRequest.serverFilename = filename }
