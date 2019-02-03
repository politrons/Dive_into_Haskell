module CategoryTheoryFunctions where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Char
import Data.List
import Data.Monoid
import Data.Either
import Control.Monad.IO.Class

-- | IO Type Monads
-- -----------------
{-| In Haskell we can say that all code section process in a pipeline with [do] it could be monad IO of type X
  We can combine monads between each other as you can see in the example bellow.
  The main monad of your program is the main IO () monad which is the runner of your program.
  You have to think that in Haskell Monads are computational context of a particular type, that when it executed
  it will generate the value type that you specify with your monad.

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

{-| Impure IO using Pure functions-}
impureUsingPure :: IO ()
impureUsingPure = do response1 <- return (getNumber3 10)
                     response2 <- return (getNumber3 response1)
                     print response2
-- | Monoid
-- --------------
{-| Monoid is a class for types which have a single most natural operator [<>] for combining values -}
monoidString :: IO()
monoidString =  print ("Hello" <> " Monoids " <> "world!!")

monoidMaybe :: IO()
monoidMaybe = print (Just "Hello" <> Just " Monoid" <> Just " Maybe" <> Just " world!!")

-- | Functor
-- --------------
{-| [Functor] in haskell implement [fmap] operator which is like map for Scala functor types, unwrap the functor and apply a function
    over the type.
    Here we create a Just String with hello and we apply fmap two times and we concat two words in the sentence.-}
justFunctor = print (fmap (\sentence -> sentence ++" world!!!")
                    (fmap (++" Functor")
                    (Just "Hello")))

functorComposition :: IO ()
functorComposition = do flatResponse <- fmap (\ number -> fmap(\number2 -> number * number2) getNumber1 ) getNumber
                        response <- flatResponse
                        print response

{-| [Functor <$> operator is sugar to map the value inside the Functor[Maybe, Optional, etc]] -}
functorDollar :: IO()
functorDollar = do maybeValue<- return $ Just "Hello functor <$> operator"
                   newMaybeValue <- return (toUpperCaseFunctor <$> maybeValue)
                   print (show newMaybeValue)

toUpperCaseFunctor:: String -> String
toUpperCaseFunctor value = map toUpper value

-- | Applicative
-- --------------
{-| Applicative is an abstraction for a context, and it has the ability to apply functions
    in the same type of context to all elements in the context -}
applicativeMaybeIO = print ((++) <$> Just "Hello" <*> Just " Applicative world")

-- | Monad
-- --------------
{-| In Haskell all the monad types as Maybe provide the [>>=] operator which is the flatMap that you can find in Scala -}
monadMaybeNumber = print (Just 1981 >>= \number -> Just (number + 100))
monadMaybeString = print (Just "Hello" >>= \word ->  Just(word ++ " Monad world!!"))
{-| FlatMap 3 where we go into the first Maybe monad 3 levels -}
monadFlatMap3 = print (Just "Hello"
                       >>= \word -> Just " Monad" --FlatMap
                       >>= \word1 -> Just " world!!" --FlatMap
                       >>= \word2 ->  Just(map toUpper (word ++ word1 ++ word2))) --FlatMap

{-| We create a Monad of type Maybe which allow us to use it instead the flatMap operator to unwrap the monad and apply
    a function over the monad -}
monadMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
monadMaybe Nothing f  = Nothing
monadMaybe (Just x) f = f x

customMonadMaybeNumber = print (Just 1981 `monadMaybe` \number -> Just (number + 100))
customMonadMaybeString = print (Just "Hello" `monadMaybe` \word ->  Just(word ++ " Monad world!!"))


{-| We create a Monad of type Either -}
runEitherMonad :: String -> Either String Int
runEitherMonad input = do
                      response <- eitherFunc1 input
                      return response

eitherFunc1 :: String -> Either String Int
eitherFunc1 "" = Left "String cannot be empty!"
eitherFunc1 str = Right $ length str

findInTwoListSameValues :: [String] -> [String] -> [String]
findInTwoListSameValues listA listB = listA >>= \letter -> filter(\letter2 -> letter == letter2) listB

-- | Do Block Vs FlatMap(>>=)
-- ----------------------------
{-| Just like in Scala we can use sugar syntax to flat over monads. Obviously for comprehension was inspired in [do block],
    Both behave exactly the same, flat a monad to extract the values to operate with them in deeper levels-}

{-| FlatMap 2 with sugar-}
doActionVsFlatMap = do { x1 <- getOperation
                       ; x2 <- getOperation1
                       ; concatString x1 x2 }

{-| FlatMap 2 without sugar-}
flatMapVsDoAction = getOperation
                                >>= \x1 -> getOperation1
                                >>= \x2 -> concatString x1 x2

--------------------------------------------------------------------------------------------------------------------

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

concatString :: String -> String -> IO String
concatString _a _b = let a=_a
                         b=_b in return (a ++ b)

getNumber :: IO Integer -- A IO monad of type Integer
getNumber = return 100

getNumber1 :: IO Integer -- A IO monad of type Integer
getNumber1 = do return 200 --No sugar the [do] operator it could be removed

getNumber2 :: Integer -> IO Integer -- Pure function that return a IO monad of type Integer
getNumber2 _number = let number = _number in return (200 * number)

getNumber3 ::Integer -> Integer -- Pure function
getNumber3 number = number * 100

sumCombinations :: Integer -> Integer
sumCombinations = do
               response1 <- curriedFunction1 -- "Extracting value" from the function f monad
               response2 <- curriedFunction2 -- "Extracting value" from the function g monad
               return (response1 + response2)

curriedFunction1 number = number + 2   -- A curried function 10 + 2
curriedFunction2 = (+3)    -- Another curried function 10 + 3

outputSumCombinations = sumCombinations 10
