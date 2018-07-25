module MonadFunctions where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Char
import Data.List


{-| In Haskell we can say that all code section process in a pipeline with [do] it could be monad IO of type X
  We can combine monads between each other as you can see in the example bellow.
  The main monad of your program is the main IO () monad which is the runner of your program.
  You have to think that in Haskell Monads are computational context of a particular type, that when it executed
  it will generate the value type that you specify with your monad.
  This is what is commonly called in Haskell world, pass from pure to impure world.

  The Do block in haskell is the original of how Scala works with sugar for comprehension to flat monads in a pipeline.
  Here we have a IO () monad which it will combine, in this for comprehension type structure of 3 other monads.
  Instead of have to flatMap everyone, the response it will be propagate in the pipeline to be used in the print
  where before we apply a function to make every word upper case |-}
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


{-| We can pass argument to [do blocks] using function with let as the definition of the variables out of the do block.
    Here as you can see we flatMap two monads and we pass the values to a third function which expect two elements.  -}
sumCompositionMonads :: IO ()
sumCompositionMonads = do
                response1 <- getNumber
                response2 <- getNumber1
                response3 <- sumNumbers response1 response2
                print response3

{-| do block where We sum the values and return a monad of type Integer -}
sumNumbers :: Integer -> Integer ->  IO Integer
sumNumbers num1 num2 = let number1 = num1
                           number2 = num2
                       in do return (number1 + number2) --No sugar the [do] operator it could be removed

{-| Here we apply the same example but instead pass to the third monad the result of the previous monads we pass
    the monads, and we flatMap the values from the monads to finally multiply the values -}
multiplyCompositionMonads :: IO ()
multiplyCompositionMonads = do
                            response <- multiplyOperations getNumber getNumber1
                            print response

multiplyOperations :: IO Integer -> IO Integer -> IO Integer
multiplyOperations ioNum1 ioNum2 = let ioNumber1=ioNum1
                                       ioNumber2 = ioNum2
                                   in do
                                        response1 <- ioNumber1
                                        response2 <- ioNumber2
                                        return (response1 * response2)

getNumber :: IO Integer -- A IO monad of type Integer
getNumber = return 100

getNumber1 :: IO Integer -- A IO monad of type Integer
getNumber1 = do return 200 --No sugar the [do] operator it could be removed


sumCombinations :: Integer -> Integer
sumCombinations = do
               response1 <- curriedFunction1      -- "Extracting value" from the function f monad
               response2 <- curriedFunction2      -- "Extracting value" from the function g monad
               return (response1 + response2)

curriedFunction1 number = number + 2   -- A curried function 10 + 2
curriedFunction2 = (+3)    -- Another curried function 10 + 3

outputSumCombinations = sumCombinations 10




--
--class Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> a -> m b -> m b