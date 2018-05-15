module CollectionFunctions where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char
import Data.List as ListUtils -- | In case you want to use an alias.

-- |   List
-- ------------

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

-- | This apeans I have a list myList and firstElement is the first element of that list. Let's put it in capital case
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

-- | Find operator from Data.List is handy to find an element in the list passing a function that does patter matching.
--  This operator it will return a Maybe type. Check Maybe module for more info

people = ["Paul","Peter", "John", "Sussan"] :: [String]

outputFind = find isJohnFunc people where isJohnFunc = \person -> person == "John"

-- |   Map
-- ------------

-- | Use operator [singleton] nn case you want to create a single key->value and keep it clear in your code.
singletonMap = Map.singleton "single_key" "single_value" :: Map[Char][Char]

-- | To create a simple map we use [fromList] operator to create a list of elements, with a tuple for (key,value)
fromListMap  = Map.fromList [("key","value"), ("Paul", "PaulValue")] :: Map[Char][Char]

-- | Add a new key->value is so simple  like use [insert] operator
newAppendMap = Map.insert "New_key" "New_value" fromListMap

-- |Size operator it will tell us the size of the map
mapSize = Map.size newAppendMap

-- |To get a value from a map we just need to use [lookup] operator. This operator it will return a Maybe type.
--  In this particular example "Just Paul"
getMapValueByKey = \name -> Map.lookup name newAppendMap
getMapValueByKeyOutput = getMapValueByKey "Paul"

isMemberPresent = \name -> Map.member name newAppendMap
isMemberPresentOutput = isMemberPresent "New_key"

-- |This operator [findWithDefault] it will try to find a key and in case cannot find it, it would use the default value passed
findWithDefaultFunc = \name -> Map.findWithDefault "Default value" "foo" newAppendMap
findWithDefaultFuncOutput = findWithDefaultFunc "FooPaul"

deleteKeyFunc = \name -> Map.delete name newAppendMap
newDeletedMap = deleteKeyFunc "Paul"

-- | Pipeline

transformValuesToUpperCase = Map.map (\element -> map toUpper element) newAppendMap
transformToUpperCaseByKey = Map.filterWithKey (\key -> \a -> key == "Paul") newAppendMap
transformToUpperCaseByValue = Map.filter (\value -> value == "PaulValue") newAppendMap

composeFunc :: Int -> String -> Map[Char][Char]
composeFunc = \_length -> \_key -> Map.map (\element -> map toUpper element) $
                                   Map.filter (\value -> length value >= _length) $
                                   Map.filterWithKey (\key -> \a -> key == _key) newAppendMap

mapFoundByKeyAndLength = composeFunc 5 "Paul"
mapNotFoundByKeyAndLength = composeFunc 10 "Paul"

-- | Zip
--  -------

-- |[zip] operator allow us to combine two list and return a list of tuples with values from every index.
list1 = [1,2,3,4,5] :: [Int]
list2 = [6,7,8,9,10] :: [Int]

zipList = zip list1 list2

-- |[zipWith] operator allow us something similar than zip but instead create a tuple from every index element we pass
-- a function that where we specify what we want to do with every two elements in every iteration.
zipWithOutput = zipWith (\a -> \b -> a + b) list1 list2

tupleList = [(1,4),(2,5),(3,6)]

-- | [unzip] operator get every element of the tuple and put array in the corresponding index position.
unzipOutput = unzip tupleList


