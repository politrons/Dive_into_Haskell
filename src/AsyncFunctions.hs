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

-- | [Concurrently] allow us execute two operations in parallel, and once we have both of them finish
--  we can return a tuple of types defined in the actions
concurrentOutput = do
    res <- concurrently getOperation2 getOperation3
    print (res :: (Int, String))

-- | [Race] operator is just like the ScalaZ race operator, is running two operations in multiple threads, and the first one
-- that finish win and the other operation is cancelled.
-- as a result we receive an [Either] of the two possible types defined in the operations.
raceOutput = do
    res <- race getOperation2 getOperation3
    print (res :: Either Int String)

-- | [forkIO] operator is used to run a do block in an other thread.
-- | Here we use an empty MVar, we run a new thread with fork and we listen to the MVar for a new value.
-- | Then from the main thread we put a value into the variable and since is subscribed from the other thread,
-- | we're able to print the value.
communicateBetweenThreads = do
  mainThreadId <- myThreadId
  input <- newEmptyMVar -- Create an empty MVar
  forkIO $ do
    inputOtherThread <- takeMVar input
    threadId <- myThreadId
    putStrLn ("Yes? " ++ " " ++ show inputOtherThread ++ " from: " ++ show threadId)
  putStrLn ("Knock knock from: " ++ show mainThreadId)
  putMVar input "Hello this is Paul!"
  threadDelay 1000000

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



