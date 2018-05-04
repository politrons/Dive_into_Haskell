module ConditionsFunction
    (
     logicOutput
    ) where

messageFunc :: String -> String -> String

-- | In this example we show how a simple nested if works
messageFunc name surname= if(name =="Paul")
                   then "Hey " ++ name ++ " how you doing!"
                   else if(surname == "Perez")
                        then surname ++".... Ah!, I know your family"
                   else "Do I know you?"

-- | In this example we show how if else works more similar to pattern matching.
messageCaseFunc name surname
             |  name == "Paul" = "Hey " ++ name ++ " how you doing!"
             |  surname == "Perez" = surname ++".... Ah!, I know your family"
             |  otherwise = "Do I know you?"

--logicOutput = putStrLn (messageFunc "Paul" "Perez")
--logicOutput = putStrLn (messageFunc "John" "Perez")
--logicOutput = putStrLn (messageFunc "John" "Smith")

logicOutput = putStrLn (messageCaseFunc "Paul" "Perez")
--logicOutput = putStrLn (messageCaseFunc "John" "Perez")
--logicOutput = putStrLn (messageCaseFunc "John" "Smith")
