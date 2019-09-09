module Main where

import Parser
import Eval 

import System.Environment (getArgs)

main :: IO () 
main = do 
  args <- getArgs 

  text <- if null args 
            then getContents 
            else concat <$> mapM readFile args 

  putStrLn $ case parseString text of 
               Right p -> show $ run p 
               Left err -> show err 