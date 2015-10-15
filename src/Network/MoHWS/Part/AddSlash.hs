{- |
Copyright: 2009, Henning Thielemann

When we get a request for http://foo.com/bar,
where 'bar' is a directory and contains an index.html,
we need to send back a redirect for http://foo.com/bar/
(i.e. add the final slash),
otherwise relative links from index.html will be relative to http://foo.com/
instead of http://foo.com/bar/.
E.g. look at http://www.haskell.org/happy/.
-}
module Network.MoHWS.Part.AddSlash (Configuration, desc, ) where

import qualified Network.MoHWS.Module as Module
import qualified Network.MoHWS.Module.Description as ModuleDesc
import qualified Network.MoHWS.Server.Request as ServerRequest
import qualified Network.MoHWS.Server.Context as ServerContext
import qualified Network.MoHWS.HTTP.Header as Header
import qualified Network.MoHWS.HTTP.Request  as Request
import qualified Network.MoHWS.HTTP.Response as Response
import qualified Network.MoHWS.Stream as Stream
import qualified Network.URI as URI

import qualified Network.MoHWS.Configuration as Config
import qualified Network.MoHWS.Configuration.Accessor as ConfigA
import qualified Network.MoHWS.Configuration.Parser as ConfigParser
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.Basic ((.>))

import Control.Monad.Trans.Maybe (MaybeT, )
import Control.Monad (guard, )
import Network.MoHWS.Utility (hasTrailingSlash, statFile, )

import System.Posix (isDirectory, )


desc :: (Stream.C body) => ModuleDesc.T body Configuration
desc =
   ModuleDesc.empty {
      ModuleDesc.name = "add slash",
      ModuleDesc.load = return . funs,
      ModuleDesc.configParser = parser,
      ModuleDesc.setDefltConfig = const defltConfig
   }

data Configuration =
   Configuration {
      addSlash_ :: Bool
   }

defltConfig :: Configuration
defltConfig =
   Configuration {
      addSlash_ = True
   }

addSlash :: Accessor.T Configuration Bool
addSlash =
   Accessor.fromSetGet (\x c -> c{addSlash_ = x}) addSlash_

parser :: ConfigParser.T st Configuration
parser =
   ConfigParser.field "addslash" p_addSlash

p_addSlash :: ConfigParser.T st Configuration
p_addSlash =
   ConfigParser.set (ConfigA.extension .> addSlash) $ ConfigParser.bool

funs :: (Stream.C body) =>
   ServerContext.T Configuration -> Module.T body
funs st =
   Module.empty {
      Module.handleRequest = handleRequest st
   }

handleRequest :: (Stream.C body) =>
   ServerContext.T Configuration -> ServerRequest.T body -> MaybeT IO (Response.T body)
handleRequest st req =
   let conf = ServerContext.config st
       uri = Request.uri $ ServerRequest.clientRequest req
       path = URI.uriPath uri
   in  do guard $ addSlash_ $ Config.extension conf
          guard =<< (fmap isDirectory $ statFile $ ServerRequest.serverFilename req)
          guard $ not $ hasTrailingSlash $ path
          return $ redirectResponse conf $ uri{URI.uriPath=path++"/"}

redirectResponse :: (Stream.C body) =>
   Config.T Configuration -> URI.URI -> Response.T body
redirectResponse conf =
   Response.makeMovedPermanently
      conf
      (Header.group [Header.makeContentType "text/plain"])
      (Response.bodyWithSizeFromString $
       Stream.fromString 100 "add trailing slash to directory path")
