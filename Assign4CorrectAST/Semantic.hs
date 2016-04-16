module Semantic where

import AST
import SymbolTable as S
import IRDataType
import ST


typeProg :: AST2 -> Bool
typeProg (M_prog (decls,stmts)) = truthVal where
    truth1 = checkDecls decls
    truth2 = True
    truthVal = truth1 && truth2


checkDecls :: [M_decl] -> Bool
checkDecls decs = truthVal where
	truthVal = foldl(\truth x -> checkDecl x) True decs

checkDecl :: M_decl -> Bool
checkDecl dec = case dec of
	M_var(str,listExp,typ) -> checkSameMexpr listExp typ
	M_fun(name,listPars,retTyp,decs,stmts) -> True
	
checkSameMexpr :: [M_expr] -> M_type -> Bool
checkSameMexpr exp typ = case typ of
    M_int -> foldl(\truth x -> checkMival x) True exp
    M_bool -> foldl(\truth x -> checkMbval x) True exp
   -- M_real -> foldl(\trush x -> checkMrval x) True exp


checkMival :: M_expr -> Bool
checkMival x = case x of
    M_ival x -> True
    _ -> False

checkMbval :: M_expr -> Bool
checkMbval x = case x of
    M_bval x -> True
    _ -> False
{-
checkMrval :: M_expr -> Bool
checkMrval x = case x of
    M_rval x -> True
    _ -> False-}
