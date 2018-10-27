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
  = Close {users::[User], error:: Integer}
  | Open {users::[User], time:: Integer}
  | HalfOpen {users::[User], error:: Integer}
  deriving (Show)

{-| Circuit breaker states implementation with instruction of what to depending the state and input of the function -}
selectAllCassandraUserWithCircuitBreaker :: CircuitBreakerState -> IO CircuitBreakerState

-- | Check if we can go to the HalfOpen
selectAllCassandraUserWithCircuitBreaker (Open users time) = checkState (Open users time)
-- | We reach the maximum number of errors so we change state to open
selectAllCassandraUserWithCircuitBreaker (Close users 5) = changeStateToOpen
-- | We change state to open
selectAllCassandraUserWithCircuitBreaker (HalfOpen users 5) = changeStateToOpen
-- | Since we are Close state, We go to Cassandra connector to get the [User]. To control error we use catch/handler error handling
selectAllCassandraUserWithCircuitBreaker (Close users errors) = do newUsers <- catch (selectAllCassandraUser) handler
                                                                   state <- checkState (Close newUsers errors)
                                                                   return state
                                                                   where
                                                                      handler :: SomeException -> IO [User]
                                                                      handler ex = return $ [User 0 "Connnector error"]

-- | Since we are Half-open state, We try to go to Cassandra connector to get the [User]. if we fail again we will pass again to open state
selectAllCassandraUserWithCircuitBreaker (HalfOpen users 4) = do newUsers <- catch (selectAllCassandraUser) handler
                                                                 state <- checkState (HalfOpen newUsers 4)
                                                                 return state
                                                                 where
                                                                      handler :: SomeException -> IO [User]
                                                                      handler ex = return $ [User 0 "Connnector error"]

{-| Circuit breaker check states implementation with instruction of what to depending the state and input of the function -}
checkState :: CircuitBreakerState -> IO CircuitBreakerState

{-| Function with pattern matching to check if with [Close] state the array of users contains [User 0 "Connector error"] which means error.
    Otherwise we just return the current state-}
checkState (Close [User 0 "Connnector error"] errors) = return $ Close [User 0 "Comnnector error"] $ errors + 1
checkState (Close users errors) = return $ Close users errors

{-| Function with pattern matching to check if with [HalfOpen] state the array of users contains [User 0 "Connector error"]
    then we change the state to [Open] again and we reset the slice time. Otherwise we change the state to [Close]-}
checkState (HalfOpen [User 0 "Connnector error"] errors) = changeStateToOpen
checkState (HalfOpen users errors) = changeStateToClose

{-| Function with pattern matching to check if lapsed time after open the circuit breaker has pass.
    if the slice time has pass we change the state to [HalfOpen] and we try to connect again. Otherewise we keep the current state-}
checkState (Open users time) =  do currentTime <- getCurrentTimeMillis
                                   if currentTime > (time + 10)
                                      then selectAllCassandraUserWithCircuitBreaker changeStateToHalfOpen
                                      else return (Open users time)

{-| We create the new state of the Circuit breaker to open-}
changeStateToOpen :: IO CircuitBreakerState
changeStateToOpen = do currentTime <- getCurrentTimeMillis
                       return $ Open [User 0 "Circuit breaker open, fail fast"] currentTime

{-| We create the new state of the Circuit breaker to open-}
changeStateToClose :: IO CircuitBreakerState
changeStateToClose = do return $ Close [] 0

{-| We create the new state of the Circuit breaker to halfOpen-}
changeStateToHalfOpen :: CircuitBreakerState
changeStateToHalfOpen = (HalfOpen [User 0 "Circuit breaker in half open, fail fast"] 4)

{-| Function to get the current time using [getCurrentTime] function and transform to int using
    [floor] and [utctDayTime]-}
getCurrentTimeMillis :: IO Integer
getCurrentTimeMillis = do
        currTime <- getCurrentTime
        let timed = floor $ utctDayTime currTime :: Integer
        return timed