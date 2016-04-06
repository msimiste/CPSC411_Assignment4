module Main where

import LexAssign
import ParAssign
import ErrM
import SkelAssign
import SymbolTable2
import Text.Show.Pretty

import System.Environment

main = do
    args <- getArgs
    let fname = args !! 0
    fconts <- readFile fname
    let tokens = myLexer fconts
    let ptree = pProg tokens
    case ptree of
        Ok tree -> do
            let astree = transProg tree
            let symbT = beginProcess astree                       
            --putStrLn $ (ppShow) astree
            putStrLn $ (ppShow) symbT
			 
        Bad emgs -> putStrLn emgs
