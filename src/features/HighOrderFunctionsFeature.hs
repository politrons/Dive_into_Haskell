module HighOrderFunctionsFeature where

import Data.Char
import Data.List

-- |In Haskell You have two ways to define functions. With and without lambdas, if you go with without
-- approach you have to define all input arguments before the = and then you can use it in your function
--      function param1 param2 = "hello" ++ param1 ++ " Haskell rocks" ++ param2r

-- | We create a lambda functions that receive variable String name and return String
highOrderFunc = (\name -> (\sentence -> name ++ " " ++sentence ++ " Combining")) :: String -> (String -> String)
--highOrderFunc name sentence = name ++ " " ++sentence ++ " Combining high order functions"

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- if the first list is contained, wholly and intact,anywhere within the second.
containsFunc = (\sentence -> if(isInfixOf "Combining" sentence )
                             then sentence ++ " high order functions"
                             else "Nothing")  :: String -> String

-- |Function that receive a String param and response String
upperCaseFunc = (\sentence -> map toUpper sentence ++ "!!!") :: String -> String

-- |Here we compose three functions.
-- We can compose functions just wrapping with func(func(func(value))) or just use [$] separator.

outputSentence = upperCaseFunc (containsFunc (highOrderFunc "Hello" "Paul"))

outputSentenceSeparator = upperCaseFunc $
                 containsFunc  $
                 highOrderFunc "Hello" "Paul"

-- |In Haskell is even more natural use Partial functions than in Scala. When we apply a function in case does not receive
--  all arguments need it to executed, it just return another function.
partialFunc = (\a -> \b -> a + b) :: Int -> Int -> Int

subFunc = partialFunc  10 -- This will return another function.

outputFunc = subFunc 20


