module Main where

import ArithmeticFunction
import FunctionsFeature
import HighOrderFunctionsFeature
import ConditionsFunction
import CollectionFunctions
import PipelineFunctions
import TuplesFunctions
import DataTypeFunctions

-- | This is the main entry program for Haskell, just like static void main or Java
--   You can only have one main declaration, so you have to point to one output or another
main :: IO ()

-- | Functions
-- ------------

--main = putStrLn welcomeSentence

-- | High order function
-- ----------------------

main = putStrLn outputSentence

-- | Pipeline
-- -------------

--main = print pipeline

-- | Conditions
-- -------------

--main = print (outputValue "Paul")
--main = putStrLn (messageFunc "Paul" "Perez")
--main = putStrLn (messageFunc "John" "Perez")
--main = putStrLn (messageFunc "John" "Smith")
--main = putStrLn (messageCaseFunc "Paul" "Perez")
--main = putStrLn (messageCaseFunc "John" "Perez")
--main = putStrLn (messageCaseFunc "John" "Smith")

-- | Arithmetic
-- -------------

--main = numericOutput
--main = print subtractResponse

-- | Tuple
--  --------

--main  = print sorterNameOutput
--main  = print olderOutput

-- | Collections
--  -------------

--main = print multiValueList
--main = putStrLn ("The length of the list is " ++ (show (length multiValueList)))
--main = print firstElementOutput
--main = print addValueListOutput
--main = print reverseListOutput
--main = print firstListOutput
--main = print lastListOutput
--main = print findElementByIndex
--main = print joinListOutput
--main = print mapListOutput
--main = print filterListOutput
--main = print isElementThere

-- | Data types
-- ------------

--main = print outputMenu