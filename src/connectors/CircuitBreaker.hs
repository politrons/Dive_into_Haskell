module CircuitBreaker where

--http://hackage.haskell.org/package/http-client-0.5.13.1/docs/Network-HTTP-Client.html
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import ModelTypes

import CassandraConnector
import Control.Exception (evaluate,try)
import Control.Concurrent (MVar)

import Data.IORef

variable :: IO (IORef Integer)
variable = newIORef 1

mutate = do
  input <- variable
  writeIORef input 10
  output <- readIORef input
  putStrLn $ show output

--testCounter::IO()
--testCounter = do errorCounter <- return (errorCounter + 1)
--                 print errorCounter

--selectAllCassandraUserCB :: IO [User]
--selectAllCassandraUserCB = do
--                    eitherResult <- try (evaluate (selectAllCassandraUser))
--                    result <- case eitherResult of
--                              Right(users) -> return users
--                              Left(e) -> do errorCounter <- return (errorCounter + 1)
--                                            return [User 0 "User not found"]
--                    return result









