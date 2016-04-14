module IRGen where

import AST
import SymbolTable as S
import IRDataType

transProg :: AST-> I_prog
transProg M_prog(mdec,mstmt)= I_prog (fcnList, len , stmts)where
	
    fcnList' = filter not.isVar(mdec)
    len = length (filter isVar(mdec))
    st = beingProcess M_prog(mdec,mstmt)
    fcnList = transMdecl fcnList'
    
    
transMdecls :: ST -> [M_decl] -> [I_fbody]
transMdecls st [] = []
transMdecls st (x:xs) = case x of
    M_fun x -> (transMdecl st x):(transMdecls st xs)
  --  M_fun (name,trips,retType,dec,stm) -> fList where
    --fList = S.look_up
	
transMdecl :: ST -> M_decl -> I_fbody
transMdecl st (name,triple,rTyp,dec,stm) = fbdy where
        (lev,nam,list,typ) = S.look_up st lbl
        fbdy = (nam,iFcns,numVars,numArgs,stmts)
        numVars = length(isVar (triple
	
isVar :: M_decl -> bool
isVar m = case m of
	M_var -> True
	_ -> False
	
convertMfun :: M_fun -> I_fun
convertMfun M_fun(name,triple,typ,decls,stmts)
