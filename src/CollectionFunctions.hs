module CollectionFunctions(collectionOutput) where

import Data.Char

-- | Here we cover how we can create collections in Haskell
myEmptyList = []
myList = "Paul"  : []
multiValueList = ["Paul","John","Esther"] :: [String]

-- | With ++ operator we can add a new element in the list as long as is another collection
addValueListOutput =  multiValueList ++ ["More"]

-- | This function get the array as input and output the first element of the list or the default value
firstOrEmpty :: [String] -> String
firstOrEmpty []     = "default"
-- | This means I have a list myList and firstElement is the first element of that list. Let's put it in capital case
firstOrEmpty (firstElement:myList)  = map toUpper firstElement
firstElementOutput = firstOrEmpty multiValueList

--collectionOutput = print multiValueList
--collectionOutput = putStrLn ("The length of the list is " ++ (show (length multiValueList)))
--collectionOutput = print firstElementOutput
collectionOutput = print addValueListOutput
