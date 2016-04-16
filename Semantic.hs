module Semantic where

import AST
import SymbolTable as S
import IRDataType
import ST


typeProg :: ST -> AST -> Bool
typeProg st (M_prog (decls,stmts)) = truthVal where
    truth1 = checkDecls st decls
    truth2 = True
    truthVal = truth1 && truth2


checkDecls :: ST -> [M_decl] -> Bool
checkDecls st decs = truthVal where
    truthVal = foldl(\truth x -> checkDecl st x) True decs

checkDecl :: ST -> M_decl -> Bool
checkDecl st dec = case dec of
    M_var(str,listExp,typ) -> checkSameMexpr listExp typ
    M_fun(name,listPars,retTyp,decs,stmts) -> truthVal where
        truth1 = checkDecls st decs
        truth2 = True --checkStmts stmts
        truth3 =  case (S.look_up st name) of 
			I_FUNCTION(_,_,funPars,_) -> compParams listPars funPars
			x -> error ("Semantic error checkDecl")
       
			
        truthVal = truth1 && truth2 && truth3
    
checkSameMexpr :: [M_expr] -> M_type -> Bool
checkSameMexpr exp typ = case typ of
    M_int -> foldl(\truth x -> checkMival x) True exp
    M_bool -> foldl(\truth x -> checkMbval x) True exp
    M_real -> foldl(\trush x -> checkMrval x) True exp


checkMival :: M_expr -> Bool
checkMival x = case x of
    M_ival x -> True
    _ -> False

checkMbval :: M_expr -> Bool
checkMbval x = case x of
    M_bval x -> True
    _ -> False

checkMrval :: M_expr -> Bool
checkMrval x = case x of
    M_rval x -> True
    _ -> False

compParams :: [(String,Int,M_type)] -> [(M_type,Int)] -> Bool
compParams [] [] = True
compParams _ [] = False
compParams [] _ = False
compParams  ((_,num1,typ1):ls1) ((typ2,num2):ls2) = (num1 == num2) && (typ1 == typ2) && (compParams ls1 ls2)
