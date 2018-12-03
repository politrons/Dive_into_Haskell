module RestConnector where

--http://hackage.haskell.org/package/http-client-0.5.13.1/docs/Network-HTTP-Client.html

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString (pack,unpack)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Exception (SomeException,try,evaluate)
import Network.HTTP.Client (Response)

simpleGetRequest :: String -> IO ByteString
simpleGetRequest uri = do
                    manager <- newManager defaultManagerSettings
                    request <- parseRequest uri
                    response <- makeRequest manager request
                    return $ responseBody response

makeRequest :: Manager -> Request -> IO (Response ByteString)
makeRequest manager request =  httpLbs request manager


--getResponse :: Response ByteString -> IO String
--getResponse response =  do eitherResult <- try (evaluate (responseBody response))::IO (Either SomeException  ByteString)
--                           response <- case eitherResult of
--                                       Left ex -> return $ "Error finding movie: " ++ show ex
--                                       Right value -> return $ "Movie found it"
--                           return response


--initResponse :: Request -> Manager -> IO (Response String)
--initResponse req man = withResponse req man $ \res -> do
--                       return res { responseBody = "" }

--initResponse ::String -> Response String
--initResponse = Response { responseBody = "" }

--makeRequest :: Manager -> Request -> IO (Response ByteString)
--makeRequest manager request =  do eitherResult <- try (evaluate (httpLbs request manager))::IO (Either SomeException  (IO (Response ByteString)))
--                                  response <- case eitherResult of
--                                       Left ex -> return $ "Error finding movie: " ++ show ex
--                                       Right value -> value
--                                  return response

--data ByteString = Empty | Chunk {-# UNPACK #-} !S.ByteString ByteString
--    deriving (Typeable)

--data Response body = Response
--    { responseStatus :: Status
--    -- ^ Status code of the response.
--    --
--    -- Since 0.1.0
--    , responseVersion :: HttpVersion
--    -- ^ HTTP version used by the server.
--    --
--    -- Since 0.1.0
--    , responseHeaders :: ResponseHeaders
--    -- ^ Response headers sent by the server.
--    --
--    -- Since 0.1.0
--    , responseBody :: body
--    -- ^ Response body sent by the server.
--    --
--    -- Since 0.1.0
--    , responseCookieJar :: CookieJar
--    -- ^ Cookies set on the client after interacting with the server. If
--    -- cookies have been disabled by setting 'cookieJar' to @Nothing@, then
--    -- this will always be empty.
--    --
--    -- Since 0.1.0
--    , responseClose' :: ResponseClose
--    -- ^ Releases any resource held by this response. If the response body
--    -- has not been fully read yet, doing so after this call will likely
--    -- be impossible.
--    --
--    -- Since 0.1.0
--    }
--    deriving (Show, Eq, T.Typeable, Functor, Data.Foldable.Foldable, Data.Traversable.Traversable)