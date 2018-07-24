module MonadFunctions where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Char
import Data.List


-- | In Haskell we can say that all code section process in a pipeline with [do] is a monad IO of type X
-- | We can combine monads between each other as you can see in the example bellow.
-- | The main monad of your program is the main IO () monad which is the runner of your program.
-- | You have to think that in Haskell Monads are computational context of a particular type that when it will
-- | be executed it will generate the type that you specify with your monad.
-- | This is what is commonly called in Haskell world, pass from pure to impure world.

-- | Here we have a IO () monad which it will combine in this for comprehension type structure 3 other monads.
-- | Instead of have to flatMap everyone the response it will be propagate in the pipeline to be used in the print
-- | where before we apply a function to make every word upper case
composeMonads :: IO ()
composeMonads = do
                response1 <- getOperation
                response2 <- getOperation1
                response3 <- getOperation2
                print (map toUpper response1 ++ " " ++ map toUpper response2 ++ " " ++ map toUpper response3)

getOperation :: IO String -- A IO monad of type String
getOperation = do
           threadDelay 1000000
           return "hello"

getOperation1 :: IO String -- A IO monad of type String
getOperation1 = do
           threadDelay 1000000
           return "Haskell"

getOperation2 :: IO String -- A IO monad of type String
getOperation2 = do
           threadDelay 1000000
           return "World!!!"

composeMonads2 :: IO ()
composeMonads2 = do
                response1 <- getNumber
                response2 <- getNumber1
--                response3 <- multiplyOperation 100
                print (response1 + response2)

getNumber :: IO Integer -- A IO monad of type String
getNumber = do return 100

getNumber1 :: IO Integer -- A IO monad of type String
getNumber1 = do return 200

--multiplyOperation :: IO Integer -> IO Integer
--multiplyOperation = \number -> do number where number * 100

--
--class Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> a -> m b -> m b