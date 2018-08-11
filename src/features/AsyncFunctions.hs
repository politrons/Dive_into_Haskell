module AsyncFunctions where

-- stack script --resolver lts-8.22
import Control.Concurrent
import Control.Concurrent.Async
import Data.Char
import Data.List
import System.Random


newRand = randomRIO (0, 100 :: Int)


-- | Sync operation
-- -----------------------
{-| In do block of Haskell by default operations are done sequentially in this example we can see how
 applying some delays in operations finish after that delay time in a sequential order--}
syncOperation :: String -> IO String
syncOperation input = do
    delay <- getStdRandom (randomR (1000000,1001000))
    _ <- threadDelay delay
    threadId <- myThreadId
    print ("Running " ++ input  ++ " in thread: " ++ show threadId)
    return input

-- runs n operations synchronously
sync :: Int -> IO ()
sync 0 = return () -- If we reach 0 this function it will be called
sync number = do
    _ <- syncOperation (show number)
    sync (pred number)

-- | Async operator
-- ---------------------
{-| With async operator we ensure the operation is executed in another thread.
    Here we wait and block for the resolution of the execution until we get the result from the other thread.
    We can also of course compose multiple results as we can see in the second code base. |-}
asyncResponse :: IO ()
asyncResponse = do
                resAsync <- async getOperation
                response <- wait resAsync
                print response

multipleAsyncResponse :: IO ()
multipleAsyncResponse = do
                        resAsync1 <- async getOperation -- Run the operation in a new thread
                        resAsync2 <- async getOperation3 -- Run the operation in a new thread
                        response1 <- wait resAsync1 -- Wait for the other thread to finish
                        response2 <- wait resAsync2 -- Wait for the other thread to finish
                        print ( map toUpper response1, response2)

{-| Another example here we use also async operators, passing two monads to a function that expect to
    receive this two monad strings and then we use the async and wait for the results.-}
combiningAsyncResponse :: IO ()
combiningAsyncResponse = do
                        response <- getMonadsOperation getOperation getOperation3
                        print response


getMonadsOperation :: IO String -> IO String -> IO String
getMonadsOperation word1 word2 = let ioWord1 = word1
                                     ioWord2 = word2
                                 in do
                                     asyncResponse1 <- async ioWord1
                                     asyncResponse2 <- async ioWord2
                                     response1 <- wait asyncResponse1
                                     response2  <- wait asyncResponse2
                                     return (response1 ++ " " ++ response2)

{-| It is also possible combine in a do block io monads ans values -}
combiningAsyncResponse1 :: IO ()
combiningAsyncResponse1 = do
                          resAsync2 <- async getOperation3
                          response2 <- wait resAsync2
                          response <- getMonadsOperation1 getOperation response2
                          print response

getMonadsOperation1 :: IO String -> String -> IO String
getMonadsOperation1 word1 word2 = let ioWord1 = word1
                                      _word2 = word2
                                 in do
                                     asyncResponse1 <- async ioWord1
                                     response1 <- wait asyncResponse1
                                     return (response1 ++ " " ++ _word2)

{-| Having monads we can also use [fmap] operator to transform the value inside the monad -}
fmapNumberAsync :: IO ()
fmapNumberAsync = do
             resAsync1 <- async getOperation2 -- Run the operation in a new thread
             response1 <- wait (fmap (\number -> number + 100) resAsync1) -- Wait and transform the value that wrap the monad
             print response1

{-| In this example using again [fmap] we get the value from the first thread element and we combine in the second one -}
fmapSentenceAsync :: IO ()
fmapSentenceAsync = do
                   resAsync1 <- async getOperation
                   resAsync2 <- async getOperation3
                   response2 <- wait resAsync2
                   -- Wait and transform the value that wrap the monad
                   response1 <- wait (fmap (\sentence -> map toUpper sentence ++ " " ++ response2) resAsync1)
                   print response1


-- | Concurrently operator
-- -----------------------
{-|  [Concurrently] allow us execute two operations in parallel, and once we have both of them finish
    we can return a tuple of types defined in the actions |-}
concurrentOutput = do
                  res <- concurrently getOperation2 getOperation3
                  print (res :: (Int, String))

-- | Race operator
-- -----------------------
{-| [Race] operator is just like the ScalaZ race operator, is running two operations in multiple threads, and the first one
    that finish win and the other operation is cancelled.
    as a result we receive an [Either] of the two possible types defined in the operations. |-}
raceOutput = do
            res <- race getOperation2 getOperation3
            print (res :: Either Int String)

-- | ForkIO operator
-- -----------------------
{-| [forkIO] operator is used to run a do block in another thread.
    Here we use an empty MVar, we run a new thread with fork and we listen to the MVar for a new value.
    Then from the main thread we put a value into the variable and since is subscribed from the other thread,
    we're able to print the value in the fork thread. |-}
forkIOThreads = do
  mainThreadId <- myThreadId -- Return information of the thread
  input <- newEmptyMVar -- Create an empty MVar
  forkIO $ do
    inputOtherThread <- takeMVar input
    threadId <- myThreadId
    putStrLn ("Yes? " ++ " " ++ inputOtherThread ++ " from: " ++ show threadId)
  putStrLn ("Knock knock from: " ++ show mainThreadId)
  putMVar input "Hello this is Paul!"
  threadDelay 1000000


-- | ForkIO operator
-- -----------------------
{-| [forkOS] unlike the ForkIO use the Operation system threads. |-}
forkOSThreads = do
  mainThreadId <- myThreadId -- Return information of the thread
  input <- newEmptyMVar -- Create an empty MVar
  forkOS $ do
    inputOtherThread <- takeMVar input
    threadId <- myThreadId
    putStrLn ("Yes? " ++ " " ++ inputOtherThread ++ " from: " ++ show threadId)
  putStrLn ("Knock knock from: " ++ show mainThreadId)
  putMVar input "Hello this is Paul!"
  threadDelay 1000000



getOperation :: IO String -- an IO monad of type String
getOperation = do
           delayTime <- getRandomNumber
           threadDelay delayTime
           threadId <- myThreadId
           print ("Running operation in thread: " ++ show threadId)
           return "hello async world!!"

getOperation2 :: IO Int -- an IO monad of type Int
getOperation2 = do
            delayTime <- getRandomNumber
            threadDelay delayTime
            threadId <- myThreadId
            print ("Running operation2 in thread: " ++ show threadId)
            return 1981

getOperation3 :: IO String
getOperation3 = do
            delayTime <- getRandomNumber
            threadDelay delayTime
            threadId <- myThreadId
            print ("Running operation3 in thread: " ++ show threadId)
            return "haskell rocks!"


getRandomNumber :: IO Int
getRandomNumber =  randomRIO (500000, 1000000 :: Int) -- Random time from 500 to 1000 ms

