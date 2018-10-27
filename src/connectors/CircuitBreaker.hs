module CircuitBreaker where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import ModelTypes

import CassandraConnector
import Control.Exception (evaluate,try,SomeException,catch)
import Control.Concurrent (MVar)

import Data.IORef
import Data.Char (digitToInt)
import Control.Monad.IO.Class (liftIO)

{-| Circuit breaker states types-}
data CircuitBreakerType
  = Close {users::[User], error:: Integer}
  | Open {users::[User], error:: Integer}
  | HalfOpen {users::[User], error:: Integer}
  deriving (Show)

{-| Circuit breaker states implementation with instruction of what to depending the state and input of the function -}
selectAllCassandraUserCB :: CircuitBreakerType -> IO CircuitBreakerType
-- | Check if we can go to the HalfOpen
selectAllCassandraUserCB (Open users errors) = return $ Open users errors
-- | We change state to open
selectAllCassandraUserCB (Close users 5) = return $ Open users 0
-- | We go to Cassandra connector to get the [User]. To control error we use catch/handler error handling
selectAllCassandraUserCB (Close users errors) = do users <- catch (selectAllCassandraUser) handler
                                                   state <- checkNumberOfUser (Close users errors) users
                                                   return state
                                                   where
                                                      handler :: SomeException -> IO [User]
                                                      handler ex = return $ [User 0 "user_not_found"]

{-| Function with pattern matching to check if the array of users contains [User 0 "user_not_found"] which means error -}
checkNumberOfUser :: CircuitBreakerType -> [User] -> IO CircuitBreakerType
checkNumberOfUser (Close users errors) [User 0 "user_not_found"] = return $ Close users $ errors + 1
checkNumberOfUser (Close users errors) _ = return $ Close users errors

