module IRGen where

import AST
import SymbolTable as S
import IRDataType

transProg :: AST-> I_prog
transProg M_prog(mdec,mstmt)= (fcnList, localVars, arraySpecs, stmts) where
	
	((fcnList, localVars, arraySpecs), sym) = getAllThree (n,st) mdec
	(n,st) = buildTable 0 (new_scope L_PROG empty) (mdec,mstmt)
	
 a program node consists of 
    --   (a) the list of functions declared
    --   (b) the number of local variables
    --   (c) a list of array specifications (<offset>,<list of bounds>)
    --   (d) the body: a list of statements
    
getAllThree -> (Int, S.ST) -> [M_decl] -> (([I_fbody], Int, [(Int,[I_expr)]), S.ST) 
getAllThree (n,st) m = ((fbody, num, specs), st') where
	fbody = filter not.isVar m
	num = length (filter isVar m)
	
	
	
	
	
isVar :: M_decl -> bool
isVar m = case m of
	M_var -> True
	_ -> False
	
convertMfun :: M_fun -> I_fun
convertMfun M_fun(name,triple,typ,decls,stmts)
