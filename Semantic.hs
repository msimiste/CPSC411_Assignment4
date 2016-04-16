module Semantic where

import AST
import SymbolTable as S
import IRDataType
import ST


typeProg :: ST -> AST -> Bool
typeProg st (M_prog (decls,stmts)) = truthVal where
    truth1 = checkDecls st decls
    truth2 = checkStmts st stmts
    truthVal = truth1 && truth2


checkDecls :: ST -> [M_decl] -> Bool
checkDecls st decs = truthVal where
    truthVal = foldl(\truth x -> checkDecl st x) True decs

checkDecl :: ST -> M_decl -> Bool
checkDecl st dec = case dec of
    M_var(str,listExp,typ) -> checkSameMexpr listExp typ && truthVal1 where
        truthVal1 = case (S.look_up st str) of
            I_VARIABLE a  -> True
            x -> error("Semantic error checkDecl_M_var " ++ show(x))
    M_fun(name,listPars,retTyp,decs,stmts) -> truthVal where
        truth1 = checkDecls st decs
        truth2 = True --checkStmts stmts
        truth3 =  case (S.look_up st name) of 
			I_FUNCTION(_,_,funPars,_) -> compParams listPars funPars
			x -> error ("Semantic error checkDecl_M_fun "++ show(x))
	truthVal = truth1 && truth2 && truth3
  
checkStmts :: ST -> [M_stmt] -> Bool
checkStmts st stmts = truthVal where
    truthVal = foldl(\truth x -> checkStmt st x) True stmts
    
checkStmt :: ST -> M_stmt -> Bool
checkStmt st stmt = case stmt of
    M_ass (str,expList,exp) -> truth1 where
        truth1 = case (S.look_up st str) of
            I_VARIABLE(_,_,typ,_) -> error("just a test :" ++ show(checkSameMexpr [exp] typ))
            I_FUNCTION(_,_,_,typ) -> checkSameMexpr [exp] typ
    M_while (exp,stmt) -> (checkExpr st exp) && (checkStmt st stmt)
    M_cond (exp,stmt1,stmt2) -> ((checkExpr st  exp) && (checkStmt st stmt1) && (checkStmt st stmt2))
    M_read (str,exprs) -> truth1 where
        truth1 = case(S.look_up st str) of
            I_VARIABLE(_,_,typ,_) -> checkSameMexpr exprs typ
    M_print expr -> checkExpr st expr
    M_return expr -> checkExpr st expr
    M_block (decs,stmts) -> (checkDecls st decs) && (checkStmts st stmts)
           
checkExpr :: ST -> M_expr -> Bool
checkExpr st exp = case exp of
    M_ival x -> checkMival exp
    M_rval x -> checkMrval exp
    M_bval x -> checkMbval exp
    M_size x -> True
    M_id (str,exp) -> foldl(\truth x -> checkMival x) True exp
    M_app (operation,exps) -> validateOperation st operation exps
    

validateOperation :: ST -> M_operation -> [M_expr] -> Bool
validateOperation st operate exprs = case operate of
    M_fn str -> truth1 where
        truth1 = case (S.look_up st str) of
            I_FUNCTION(_,_,list,_) -> (exprToType exprs) == (convertParams list)
    M_add -> foldl(\truth x -> checkMival x) True exprs
    M_mul -> foldl(\truth x -> checkMival x) True exprs
    M_sub -> foldl(\truth x -> checkMival x) True exprs
    M_div -> foldl(\truth x -> checkMival x) True exprs
    M_neg -> foldl(\truth x -> checkMival x) True exprs    
    M_lt -> foldl(\truth x -> checkMival x) True exprs
    M_le -> foldl(\truth x -> checkMival x) True exprs
    M_gt -> foldl(\truth x -> checkMival x) True exprs
    M_ge -> foldl(\truth x -> checkMival x) True exprs
    M_eq -> foldl(\truth x -> checkMival x) True exprs
    M_not -> foldl(\truth x -> checkMbval x) True exprs
    M_and -> foldl(\truth x -> checkMbval x) True exprs
    M_or -> foldl(\truth x -> checkMbval x) True exprs
            
exprToType ::[M_expr] -> [M_type]
exprToType [] = []
exprToType (x:xs) = case x of
    M_ival _ -> M_int:(exprToType xs)
    M_bval _ -> M_bool:(exprToType xs)
    M_rval _ -> M_real:(exprToType xs)
    _ -> error("Error in fcn params " ++ show(x))
    
convertParams :: [(M_type,Int)] -> [M_type]
convertParams [] = []
convertParams ((x,t):xs) = (x:(convertParams xs)) 

    
  
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
