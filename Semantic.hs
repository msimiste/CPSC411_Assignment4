module Semantic where

import AST
import SymbolTable as S
import IRDataType
import ST


--Starting function 
typeProg :: ST -> AST -> Bool
typeProg st (M_prog (decls,stmts)) = truthVal where
    truthVal = truth1 && truth2 --error("truth1 : "++show(truth1) ++ ("truth2 : ")++show(truth2)) truth1 && truth2
    truth1 = checkDecls st decls --error("showing decls :" ++ show(decls)) 
    truth2 = checkStmts st stmts --error("showing stmts :" ++ show(stmts)) 
    


checkDecls :: ST -> [M_decl] -> Bool
checkDecls st [] = True
checkDecls st (x:xs) = (checkDecl st x) && (checkDecls st xs)
--checkDecls st decs = truthVal where
  --  truthVal = foldl(\truth x -> checkDecl st x) True decs

checkDecl :: ST -> M_decl -> Bool
checkDecl st dec = case dec of
    M_var(str,listExp,typ) -> checkSameMexpr listExp typ && truthVal1 where
        truthVal1 = case (S.look_up st str) of
            I_VARIABLE a  -> True --error("line 24 :" ++ show(a))
            x -> error("Semantic error checkDecl_M_var " ++ show(x))
    M_fun(name,listPars,retTyp,decs,stmts) -> truthVal where
        truth1 = checkDecls st decs
        truth2 = checkStmts st stmts
        truth3 =  case (S.look_up st name) of 
            I_FUNCTION(_,_,funPars,_) -> compParams listPars funPars
            x -> error ("Semantic error checkDecl_M_fun "++ show(x))
        truthVal = truth1 && truth2 && truth3
  
checkStmts :: ST -> [M_stmt] -> Bool
checkStmts st [] = True
checkStmts st (x:xs) = (checkStmt st x) && (checkStmts st xs)

    
checkStmt :: ST -> M_stmt -> Bool
checkStmt st stmt = case stmt of
    M_ass (str,expList,exp) -> truth1 where --error("reached line 41: "++show(stmt))
        truth1 = case (S.look_up st str) of
            I_VARIABLE(_,_,typ,_) ->  checkSameMexpr (exp:expList) typ --error("reached line 44: " ++ show(checkSameMexpr [exp] typ))--checkSameMexpr [exp] typ --error("reached line 44: " ++ show(checkSameMexpr [exp] typ))
            I_FUNCTION(_,_,_,typ) -> error("just a 2nd test :" ++ show(checkSameMexpr [exp] typ))
            x -> error("showing :" ++ show(x))
    M_while (exp,stmt) -> (checkExpr st exp) && (checkStmt st stmt) --error("Reached line 46: ")
    M_cond (exp,stmt1,stmt2) -> (checkStmt st stmt1) && (checkStmt st stmt2)--error("Reached line 47:")--((checkExpr st  exp) && (checkStmt st stmt1) && (checkStmt st stmt2))--("Reached line 47:")
    M_read (str,exprs) -> truth1 where
        truth1 = case(S.look_up st str) of
            I_VARIABLE(_,_,typ,_) -> checkSameMexpr exprs typ--error("line 51 : "++show( checkSameMexpr exprs typ)) --error("Reached line 50:" ++ show (stmt))
    M_print expr -> checkExpr st expr --error("line 52 : " ++ show(checkExpr st expr)++"\n stmt: "++show(stmt)++" \nexpr: "++show(expr)) --error("Reached line 51: "++show(stmt))
    M_return expr -> checkExpr st expr -- error("Reached line 52:") 
    M_block (decs,stmts) -> (checkDecls st decs) && (checkStmts st stmts) -- error("Reached line 54:") 
    
           
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
            I_FUNCTION(_,_,list,_) -> case ((exprToType exprs) == (convertParams list)) of
                True -> True
                False -> error("error in M_fn of validateOperation : " ++ show(exprToType exprs)++ " not equal to \n"++show(convertParams list))-- error("line 71 : "++"\n exprs : "++ show(exprs)++"\n totype: " ++show(exprToType exprs)++"\n list: "++ show(list) ++ "\n totype: "++show(convertParams list))
    M_add -> case (foldl(\truth x -> checkMival x) True exprs) of 
        True -> True
        False -> error ("error in M_add of validateOperation : " ++ show(exprs))
    M_mul -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_mul of validateOperation : " ++ show(exprs))
    M_sub -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_sub of validateOperation : " ++ show(exprs))    
    M_div -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_div of validateOperation : " ++ show(exprs))
    M_neg -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_neg of validateOperation : " ++ show(exprs)) 
    M_lt -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_lt of validateOperation : " ++ show(exprs))
    M_le -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_le of validateOperation : " ++ show(exprs))
    M_gt -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_gt of validateOperation : " ++ show(exprs))
    M_ge -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_ge of validateOperation : " ++ show(exprs))
    M_eq -> case (foldl(\truth x -> checkMival x) True exprs) of
        True -> True
        False -> error ("error in M_eq of validateOperation : " ++ show(exprs))
    M_not -> case (foldl(\truth x -> checkMbval x) True exprs) of
        True -> True
        False -> error ("error in M_not of validateOperation : " ++ show(exprs))
    M_and -> case (foldl(\truth x -> checkMbval x) True exprs) of
        True -> True
        False -> error ("error in M_and of validateOperation : " ++ show(exprs))
    M_or -> case (foldl(\truth x -> checkMbval x) True exprs) of
        True -> True
        False -> error ("error in M_or of validateOperation : " ++ show(exprs))
            
exprToType ::[M_expr] -> [M_type]
exprToType [] = []
exprToType (x:xs) = case x of
    M_ival _ ->  M_int:(exprToType xs)
    M_bval _ -> M_bool:(exprToType xs)
    M_rval _ -> M_real:(exprToType xs)
    M_id (str,exps) -> (exprToType exps) ++ (exprToType xs)
    M_app (op,exps) -> (exprToType exps) ++ (exprToType xs)
   -- _ -> error("Error in fcn params line 92: " ++ show(x))
    
convertParams :: [(M_type,Int)] -> [M_type]
convertParams [] = []
convertParams ((x,t):xs) = (x:(convertParams xs)) 

 
checkSameMexpr :: [M_expr] -> M_type -> Bool
checkSameMexpr [] typ = True --error("checkSameMexpr line 101 : "++show(typ))
checkSameMexpr exp typ = case (foldl(\truth x -> x == typ) True (exprToType exp)) of
    True -> True
    False -> error("error in M_int of checkSameMexpr: \n" ++ show(exprToType exp) ++ "\n "++show(typ) ++" are not equal")

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
