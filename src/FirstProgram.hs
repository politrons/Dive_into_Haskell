module FirstProgram where

import Data.Char
import Data.List

outputProgram = do

  print "Welcome:"
  print "1. exit."
  print "2. restart."

  command <- getLine

  case command of
          "1" -> putStrLn "Exit application"
          "2" -> do
                  putStrLn "Preparing to restart...."
                  putStrLn "Restarting...."
                  outputProgram
          otherwise -> putStrLn "Command not found"




