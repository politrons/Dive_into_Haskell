module AsyncFunctions where

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

-- | Concurrently allow us execute several operations in parallel, and once we have all of them we can return  a tuple
-- of types defined in the actions
concurrentOutput = do
    res <- concurrently operation2 operation3
    print (res :: (Int, String))

-- | Race operator is just like the ScalaZ race operator, is running operations in multiple threads, and the first one
-- that finish win and the other operations are cancelled.
raceOutput = do
    res <- race operation2 operation3
    print (res :: Either Int String)

operation :: IO String
operation = do
           threadDelay 2000000 -- 2 seconds
           threadId <- myThreadId
           print ("Running operation in thread: " ++ show threadId)
           return "hello async world!!"

operation2 :: IO Int
operation2 = do
    threadDelay 500000 -- micro seconds (500 ms)
    threadId <- myThreadId
    print ("Running operation2 in thread: " ++ show threadId)
    return 1981

operation3 :: IO String
operation3 = do
    threadDelay 1000000 -- micro seconds (1000 ms)
    threadId <- myThreadId
    print ("Running operation3 in thread: " ++ show threadId)
    return "operation3 result"

communicate = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received " ++ show v)
  putStrLn "sending"
  putMVar m "wake up!"

