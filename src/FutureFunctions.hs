module FutureFunctions where

-- stack script --resolver lts-8.22
import Control.Concurrent
import Control.Concurrent.Async
import System.Random

newRand = randomRIO (0, 100 :: Int)


-- | With async operator we ensure the operation is executed in another thread.
--  Here we wait and block for the resolution of the execution until we get the result from the other thread.
asyncResponse = do
                resAsync <- async operation
                response <- wait resAsync
                print response

operation = do
           threadDelay 5000000
           return "hello async world!!"


action1 :: IO Int
action1 = do
    threadDelay 500000 -- micro seconds (500 ms)
    print "Thread 1"
    return 5

action2 :: IO String
action2 = do
    threadDelay 1000000 -- micro seconds (1000 ms)
    print "Thread 2"
    return "action2 result"

concurrentOutput = do
    res <- concurrently action1 action2
    print (res :: (Int, String))

raceOutput = do
    res <- race action1 action2
    print (res :: Either Int String)


communicate = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received " ++ show v)
  putStrLn "sending"
  putMVar m "wake up!"

