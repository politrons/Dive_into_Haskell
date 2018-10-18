module RestConnector where

--http://hackage.haskell.org/package/http-client-0.5.13.1/docs/Network-HTTP-Client.html

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

simpleGetRequest :: IO ()
simpleGetRequest = do
                    manager <- newManager defaultManagerSettings

                    request <- parseRequest "http://httpbin.org/get"
                    response <- httpLbs request manager
                    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
                    print $ responseBody response