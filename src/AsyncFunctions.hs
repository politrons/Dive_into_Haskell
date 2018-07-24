module AsyncFunctions where

-- stack script --resolver lts-8.22
import Control.Concurrent
import Control.Concurrent.Async
import System.Random

newRand = randomRIO (0, 100 :: Int)

-- | With async operator we ensure the operation is executed in another thread.
--  Here we wait and block for the resolution of the execution until we get the result from the other thread.
--  We can also of course compose multiple results as we can see in the second code base.
asyncResponse :: IO () -- A monad of type void
asyncResponse = do
                resAsync <- async getOperation
                response <- wait resAsync
                print response

multipleAsyncResponse :: IO ()
multipleAsyncResponse = do
                resAsync1 <- async getOperation
                resAsync2 <- async getOperation3
                response1 <- wait resAsync1
                response2 <- wait resAsync2
                print (response1, response2)

-- | Concurrently allow us execute several operations in parallel, and once we have all of them finish
--  we can return  a tuple of types defined in the actions
concurrentOutput = do
    res <- concurrently getOperation2 getOperation3
    print (res :: (Int, String))

-- | Race operator is just like the ScalaZ race operator, is running operations in multiple threads, and the first one
-- that finish win and the other operations are cancelled.
raceOutput = do
    res <- race getOperation2 getOperation3
    print (res :: Either Int String)

getOperation :: IO String -- an IO monad of type String
getOperation = do
           threadDelay 2000000 -- 2 seconds
           threadId <- myThreadId
           print ("Running operation in thread: " ++ show threadId)
           return "hello async world!!"

getOperation2 :: IO Int -- an IO monad of type Int
getOperation2 = do
    threadDelay 500000 -- micro seconds (500 ms)
    threadId <- myThreadId
    print ("Running operation2 in thread: " ++ show threadId)
    return 1981

getOperation3 :: IO String
getOperation3 = do
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

