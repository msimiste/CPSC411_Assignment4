module MikeIRGen where

import AST
import SymbolTable as S
import IRDataType
import ST

transProgIR :: AST -> I_prog
transProgIR (M_prog(mdec,mstmt)) = IPROG (fcnList, len , stmts) where
	fcnList' = (filter(\x -> not (isVar x))) mdec
	len = length (filter isVar(mdec))
	st = beginProcess (M_prog(mdec,mstmt))
	fcnList = transMdecls st fcnList'
	stmts = transMstmts st mstmt
    
    
transMdecls :: ST -> [M_decl] -> [I_fbody]
transMdecls st [] = []
transMdecls st (x:xs) = case x of
    M_fun x -> (transMdecl st (M_fun x)):(transMdecls st xs)
  --  M_fun (name,trips,retType,dec,stm) -> fList where
    --fList = S.look_up
	
transMdecl :: ST -> M_decl -> I_fbody
transMdecl st (M_fun(name,triple,rTyp,dec,stm)) = IFUN(name,iFcns,numVars,numArgs,stmts) where
		numArgs = (length(filter(\(s,n,t) -> n < 0) triple))
		numVars = (length triple) - numArgs
		iFcns' = (filter(\x -> not (isVar x))) dec
		iFcns = map(\x -> convertMfun st x) iFcns'
		stmts = transMstmts st stm
        
transMstmts :: ST -> [M_stmt] -> [I_stmt]
transMstmts st [] = []
transMstmts st (x:xs) = (transMstmt st x):(transMstmts st xs)



transMstmt :: ST -> M_stmt -> I_stmt
transMstmt st x = case x of
	M_ass (str,expList,exp) -> IASS(lev,off,expr) where
		expr = convertMexpr st exp
		I_VARIABLE(lev,off,_,_) = look_up st str	
		
	M_while (e,s) -> IWHILE(exp,stm) where
		exp = convertMexpr st e
		stm = transMstmt st s
	M_cond (e,s1,s2) -> ICOND(exp,stm1,stm2) where
		exp = convertMexpr st e
		stm1 = transMstmt st s1
		stm2 = transMstmt st s2
	M_read (s,exp) -> case  (S.look_up st s) of
		I_VARIABLE(lev,off,M_bool,_) -> IREAD_B(lev,off)
		I_VARIABLE(lev,off,M_int,_) -> IREAD_I(lev,off)
	
convertMexpr :: ST -> M_expr -> I_expr
convertMexpr st x = case x of
	M_ival y -> (INUM (fromInteger(y)))
	M_bval y -> (IBOOL y)
	M_id (str,_) -> IID(lev,off) where
		I_VARIABLE(lev,off,_,_) = S.look_up st str
	M_app (mOP,exp) -> IAPP (opn, expI) where
		opn = convertMop st mOP
		expI = map(\x -> convertMexpr st x) exp


convertMop :: ST -> M_operation -> I_opn
convertMop  st x = case x of
	M_fn (str) -> ICALL(lbl,lev) where
		I_FUNCTION(lev,lbl,_,_) = S.look_up st str
	M_add -> IADD
	M_mul -> IMUL
	M_sub -> ISUB
	M_div -> IDIV
	M_neg -> INEG
	M_lt  -> ILT
	M_le  -> ILE
	M_gt  -> IGT
	M_ge  -> IGE
	M_eq  -> IEQ
	M_not -> INOT
	M_and -> IAND
	M_or  -> IOR
   -- M_float
    --M_floor
    --M_ceil
	
		

isVar :: M_decl -> Bool
isVar m = case m of
	M_var m -> True
	_ -> False
	
convertMfun :: ST -> M_decl -> I_fbody 
convertMfun st (M_fun (name,triple,typ,decls,stmts)) = IFUN (name,fcnList,locV,locA,istmts) where
	Symbol_table(_,locV,locA,_) = head st
	fcnList = transMdecls st decls
	istmts = transMstmts st stmts
	
