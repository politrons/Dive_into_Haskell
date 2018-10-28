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
data CircuitBreakerState
  = Close {users::[User], errors:: Integer}
  | Open {users::[User], time:: Integer, errors::Integer}
  | HalfOpen {users::[User], errors:: Integer}
  deriving (Show)

-- | Pattern matching connector state
-- ----------------------------------
{-| Circuit breaker states implementation with instruction of what to depending the state and input of the function -}
selectAllCassandraUserWithCircuitBreaker :: CircuitBreakerState -> IO CircuitBreakerState

-- | Check if we can go to the HalfOpen
selectAllCassandraUserWithCircuitBreaker (Open users time errors) = checkState (Open users time errors)
-- | We reach the maximum number of errors so we change state to open
selectAllCassandraUserWithCircuitBreaker (Close users 5) = changeStateToOpen
-- | We change state to open
selectAllCassandraUserWithCircuitBreaker (HalfOpen users 5) = changeStateToOpen
-- | Since we are [Close] state, We go to Cassandra connector to get the [User]. To control error we use catch/handler error handling
selectAllCassandraUserWithCircuitBreaker (Close users errors) = connectToCassandra (Close users errors) getCircuitBreakerState
-- | Since we are [HalfOpen] state, We try to go to Cassandra connector to get the [User]. if we fail again we will pass again to [Open] state
selectAllCassandraUserWithCircuitBreaker (HalfOpen users 4) = connectToCassandra (HalfOpen users 4) getCircuitBreakerState

-- | Function to connect to cassandra
-- -----------------------------------
{-| For this High Order Function we use as second argument a function [getCircuitBreakerState] which it will
    return the specific state passed [Close | HalfOpen] from the invoker of the function.-}
connectToCassandra:: CircuitBreakerState -> ([User] -> Integer -> CircuitBreakerState) -> IO CircuitBreakerState
connectToCassandra state function = do newUsers <- catch (selectAllCassandraUser) handler
                                       state <- checkState $ function newUsers (errors state)
                                       return state
                                       where
                                         handler :: SomeException -> IO [User]
                                         handler ex = return $ [User 0 "Connnector error"]

getCircuitBreakerState :: [User] -> Integer -> CircuitBreakerState
getCircuitBreakerState users 4 =  (HalfOpen users 4)
getCircuitBreakerState users errors= (Close users errors)

-- | Pattern matching circuit breaker state
-- ----------------------------------------
{-| Circuit breaker check states implementation with instruction of what to depending the state and input of the function -}
checkState :: CircuitBreakerState -> IO CircuitBreakerState

{-| Function with pattern matching to check if with [Close] state the array of users contains [User 0 "Connector error"] which means error.
    Otherwise we just return the current state-}
checkState (Close [User 0 "Connnector error"] errors) = return $ Close [User 0 "Comnnector error"] $ errors + 1
checkState (Close users errors) = return $ Close users errors

{-| Function with pattern matching to check if with [HalfOpen] state the array of users contains [User 0 "Connector error"]
    then we change the state to [Open] again and we reset the slice time. Otherwise we change the state to [Close]-}
checkState (HalfOpen [User 0 "Connnector error"] errors) = changeStateToOpen
checkState (HalfOpen users errors) = changeStateToClose users

{-| Function with pattern matching to check if lapsed time after open the circuit breaker has pass.
    if the slice time has pass we change the state to [HalfOpen] and we try to connect again. Otherewise we keep the current state-}
checkState (Open users time errors) =  do currentTime <- getCurrentTimeMillis
                                          if currentTime > (time + 10)
                                             then selectAllCassandraUserWithCircuitBreaker changeStateToHalfOpen
                                             else return (Open users time errors)

-- | Util functions to change state of circuit breaker
-- ---------------------------------------------------
{-| We create the new state of the Circuit breaker to open-}
changeStateToOpen :: IO CircuitBreakerState
changeStateToOpen = do currentTime <- getCurrentTimeMillis
                       return $ Open [User 0 "Circuit breaker open, fail fast"] currentTime 0

{-| We create the new state of the Circuit breaker to open-}
changeStateToClose :: [User] ->  IO CircuitBreakerState
changeStateToClose users = do return $ Close users 0

{-| We create the new state of the Circuit breaker to halfOpen-}
changeStateToHalfOpen :: CircuitBreakerState
changeStateToHalfOpen = (HalfOpen [] 4)

{-| Function to get the current time using [getCurrentTime] function and transform to int using
    [floor] and [utctDayTime]-}
getCurrentTimeMillis :: IO Integer
getCurrentTimeMillis = do
        currTime <- getCurrentTime
        let timed = floor $ utctDayTime currTime :: Integer
        return timed
