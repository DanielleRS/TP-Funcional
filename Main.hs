module Main where

import System.Environment
import System.Exit
import Parser (parse)
import MyFunParser (cmd)
import Interpreter (execute, initialMemory, Memory)

main :: IO ()
main = getArgs >>= argParse >>= run

argParse ["--help"] = putStrLn "Usage: MyFunInterpreter [--repl ] [file]" >> exit
argParse [] = getContents
argParse fs = concat `fmap` mapM readFile fs

exit = exitWith ExitSuccess

run :: String -> IO ()
run file =
  case parse cmd file of
   Just (program, "") ->
    do
      mem <- execute initialMemory program
      putStrLn "done."
   _ -> putStrLn "erro"
