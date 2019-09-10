module Main where

import Parser
import Eval 

import Data.Map (toList)
import Data.Foldable (for_)

import Text.Printf 

import System.Environment (getArgs)

main :: IO () 
main = do 
  args <- getArgs 

  text <- if null args 
            then getContents 
            else concat <$> mapM readFile args 

  case parseString text of 
    Right p -> for_ (toList $ run p) $ \(var, val) -> 
                 printf "%s -> %d\n" var val 
    Left err -> print err 