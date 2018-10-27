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
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Clock

{-| Circuit breaker states types-}
data CircuitBreakerType
  = Close {users::[User], error:: Integer}
  | Open {users::[User], time:: Integer}
  | HalfOpen {users::[User], error:: Integer}
  deriving (Show)

{-| Circuit breaker states implementation with instruction of what to depending the state and input of the function -}
selectAllCassandraUserWithCircuitBreaker :: CircuitBreakerType -> IO CircuitBreakerType
-- | Check if we can go to the HalfOpen
selectAllCassandraUserWithCircuitBreaker (Open users time) = checkOpenState (Open users time)
-- | We change state to open
selectAllCassandraUserWithCircuitBreaker (Close users 5) = changeStateToOpen
-- | We change state to open
selectAllCassandraUserWithCircuitBreaker (HalfOpen users 5) = changeStateToOpen
-- | Since we are Close state, We go to Cassandra connector to get the [User]. To control error we use catch/handler error handling
selectAllCassandraUserWithCircuitBreaker (Close users errors) = do users <- catch (selectAllCassandraUser) handler
                                                                   state <- checkCloseState (Close users errors)
                                                                   return state
                                                                   where
                                                                      handler :: SomeException -> IO [User]
                                                                      handler ex = return $ [User 0 "user_not_found"]

-- | Since we are Half-open state, We try to go to Cassandra connector to get the [User]. if we fail again we will pass again to open state
selectAllCassandraUserWithCircuitBreaker (HalfOpen users 4) = do users <- catch (selectAllCassandraUser) handler
                                                                 state <- checkCloseState (Close users 4)
                                                                 return state
                                                                 where
                                                                      handler :: SomeException -> IO [User]
                                                                      handler ex = return $ [User 0 "user_not_found"]

{-| Function with pattern matching to check if the array of users contains [User 0 "user_not_found"] which means error.
    Otherwise we just return the current state-}
checkCloseState :: CircuitBreakerType -> IO CircuitBreakerType
checkCloseState (Close [User 0 "user_not_found"] errors) = return $ Close [] $ errors + 1
checkCloseState (Close users errors) = return $ Close users errors

checkOpenState :: CircuitBreakerType -> IO CircuitBreakerType
checkOpenState (Open users time) =  do currentTime <- getCurrentTimeMillis
                                       if currentTime > (time + 1000)
                                               then return (HalfOpen [User 0 "Circuit breaker in half open, fail fast"] 4) -- change by selectAllCassandraUserWithCircuitBreaker
                                               else return (Open [] time)

{-| We create the new state of the Circuit breaker to open-}
changeStateToOpen :: IO CircuitBreakerType
changeStateToOpen = do currentTime <- getCurrentTimeMillis
                       return $ Open [User 0 "Circuit breaker open, fail fast"] currentTime

{-| We create the new state of the Circuit breaker to open-}
changeStateToClose :: IO CircuitBreakerType
changeStateToClose = do return $ Close [] 0


{-| Function to get the current time using [getCurrentTime] function and transform to int using
    floor and utctDayTime-}
getCurrentTimeMillis :: IO Integer
getCurrentTimeMillis = do
        currTime <- getCurrentTime
        let timed = floor $ utctDayTime currTime :: Integer
        return timed