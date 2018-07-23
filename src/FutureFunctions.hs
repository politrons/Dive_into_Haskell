module FutureFunctions where

-- stack script --resolver lts-8.22
import Control.Concurrent
import Control.Concurrent.Async
import System.Random

newRand = randomRIO (0, 100 :: Int)

action1 :: IO Int
action1 = do
    threadDelay 500000 -- just to make it interesting
    return 5

action2 :: IO String
action2 = do
    threadDelay 1000000
    return "action2 result"

concurrentOutput = do
    res <- concurrently action1 action2
    print (res :: (Int, String))

raceOutput = do
    res <- race action1 action2
    print (res :: Either Int String)
