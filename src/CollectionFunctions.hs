module CollectionFunctions where

import Data.Char
import Data.List as ListUtils -- | In case you want to use an alias.

-- | Here we cover how we can create collections in Haskell
myEmptyList = []
myList = "Paul"  : []
multiValueList = ["Paul","John","Esther", "Luis","Jackson"] :: [String]

-- | Add an element to the end of a list.
addValueListOutput = multiValueList ++ ["More"]

-- | Add an element to the start of a list.

--addValueListOutput = "More" : multiValueList

-- | This function get the array as input and output the first element of the list or the default value
firstOrEmpty :: [String] -> String
firstOrEmpty []     = "default"

-- | This means I have a list myList and firstElement is the first element of that list. Let's put it in capital case
firstOrEmpty (firstElement:myList)  = map toUpper firstElement
firstElementOutput = firstOrEmpty multiValueList

-- | If we want to change the order of the list we just need to use reverse operator
reverseListOutput = reverse multiValueList

-- | Get first element of list
firstListOutput = head multiValueList

-- | Get last element of list
lastListOutput = last multiValueList

-- | Find element by index
findElementByIndex = multiValueList !! 1

-- | Join two list together
joinListOutput = ["Lets", "join"] ++ ["two", "list"]

-- | Iterate the list and put all element of list in capital case
mapListOutput = map (\element ->  map toUpper element) multiValueList

-- | Iterate the list and  use filter operator to check the length of every element and just add it in a new
filterListOutput = filter (\element ->  length element == 4)  multiValueList

-- | Check if a element is part of the collection and return a boolean
isElementThere = "Paul" `elem` multiValueList

-- | Find operator from Data.List is handy to find an element in the list passing a function that does patter matching

people = ["Paul","Peter", "John", "Sussan"] :: [String]

outputFind = find isJohnFunc people where isJohnFunc = \person -> person == "John"


