module Main where

import System.Environment
import System.Directory
import Control.Monad (void)
import Interpreter

main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then
        error "You need to specify a file."
    else do 
        fileExists <- doesFileExist $ head args
        if fileExists then do
            input <- readFile $ head args
            void $ interpret input
        else
            error "File does not exist"