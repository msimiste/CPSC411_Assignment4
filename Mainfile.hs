module Main where

import LexAssign
import ParAssign
import ErrM
import SkelAssign
import SymbolTable
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
            putStrLn $ (ppShow) astree
        Bad emgs -> putStrLn emgs