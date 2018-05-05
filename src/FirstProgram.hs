module FirstProgram(outputProgram) where

import Data.Char
import Data.List

-- | Just like in Scala we can define better business logic types from primitives using type
type Names = [Char]

multiValueList = ["Paul","John","Esther", "Luis","Jackson", "Emil"] :: [Names]

-- | Here we define our functions contracts
filterLengthFunc =  (\element ->  length element == 4) :: String -> Bool
upperCaseFunc =  (\element ->  map toUpper element) :: String -> String
appendFunc =  (\element -> "|" ++ element ++ "|") :: String -> String

-- | Here we compose functions with operators and link each other using $
outputProgram = map appendFunc $
           map upperCaseFunc $
           filter filterLengthFunc $
           drop 1  $
           sort multiValueList


