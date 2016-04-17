module Semantic where

import AST
import SymbolTable as S
import IRDataType
import ST


typeProg :: ST -> AST -> Bool
typeProg st (M_prog (decls,stmts)) = truthVal where
    truthVal = truth1 && truth2 --error("truth1 : "++show(truth1) ++ ("truth2 : ")++show(truth2)) truth1 && truth2
    truth1 = checkDecls st decls --error("showing decls :" ++ show(decls)) 
    truth2 = checkStmts st stmts --error("showing stmts :" ++ show(stmts)) 
    


checkDecls :: ST -> [M_decl] -> Bool
checkDecls st decs = truthVal where
    truthVal = foldl(\truth x -> checkDecl st x) True decs

checkDecl :: ST -> M_decl -> Bool
checkDecl st dec = case dec of
    M_var(str,listExp,typ) -> checkSameMexpr listExp typ && truthVal1 where
        truthVal1 = case (S.look_up st str) of
            I_VARIABLE a  -> True --error("line 24 :" ++ show(a))
            x -> error("Semantic error checkDecl_M_var " ++ show(x))
    M_fun(name,listPars,retTyp,decs,stmts) -> truthVal where
        truth1 = checkDecls st decs
        truth2 = True --checkStmts stmts
        truth3 =  case (S.look_up st name) of 
			I_FUNCTION(_,_,funPars,_) -> compParams listPars funPars
			x -> error ("Semantic error checkDecl_M_fun "++ show(x))
	truthVal = truth1 && truth2 && truth3
  
checkStmts :: ST -> [M_stmt] -> Bool
checkStmts st [] = True
checkStmts st (x:xs) = (checkStmt st x) && (checkStmts st xs)
--checkStmts st stmts = truthVal where
  --  truthVal =  foldl(\truth x -> checkStmt st x) True stmts--error("line 38 : " ++ show(foldl(\truth x -> checkStmt st x) True stmts))
    
checkStmt :: ST -> M_stmt -> Bool
checkStmt st stmt = case stmt of
    M_ass (str,expList,exp) -> truth1 where --error("reached line 41: "++show(stmt))
        truth1 = case (S.look_up st str) of
            I_VARIABLE(_,_,typ,_) ->  checkSameMexpr [exp] typ --error("reached line 44: " ++ show(checkSameMexpr [exp] typ))--checkSameMexpr [exp] typ --error("reached line 44: " ++ show(checkSameMexpr [exp] typ))
            I_FUNCTION(_,_,_,typ) -> error("just a 2nd test :" ++ show(checkSameMexpr [exp] typ))
            x -> error("showing :" ++ show(x))
    M_while (exp,stmt) -> error("Reached line 46: ") -- (checkExpr st exp) && (checkStmt st stmt) --error("Reached line 46: ")
    M_cond (exp,stmt1,stmt2) -> error("Reached line 47:")--((checkExpr st  exp) && (checkStmt st stmt1) && (checkStmt st stmt2))--("Reached line 47:")
    M_read (str,exprs) -> truth1 where
        truth1 = case(S.look_up st str) of
            I_VARIABLE(_,_,typ,_) -> checkSameMexpr exprs typ--error("line 51 : "++show( checkSameMexpr exprs typ)) --error("Reached line 50:" ++ show (stmt))
    M_print expr -> checkExpr st expr --error("line 52 : " ++ show(checkExpr st expr)++"\n stmt: "++show(stmt)++" \nexpr: "++show(expr)) --error("Reached line 51: "++show(stmt))
    M_return expr -> checkExpr st expr -- error("Reached line 52:") 
    M_block (decs,stmts) -> error("Reached line 54:") --(checkDecls st decs) && (checkStmts st stmts) -- error("Reached line 54:") 
    
           
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
            I_FUNCTION(_,_,list,_) -> (exprToType exprs) == (convertParams list) -- error("line 71 : "++"\n exprs : "++ show(exprs)++"\n totype: " ++show(exprToType exprs)++"\n list: "++ show(list) ++ "\n totype: "++show(convertParams list))
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
checkSameMexpr (e:exp) typ = case typ of
    M_int ->(checkMival e) && (checkSameMexpr exp typ)--foldl(\truth x -> checkMival x) True exp--(checkMival e) && (checkSameMexpr exp typ) --error("line 104 : " ++ show((checkMival e) && (checkSameMexpr exp typ)))--foldl(\truth x -> checkMival x) True exp
    M_bool ->  case ((checkMbval e) && (checkSameMexpr exp typ)) of 
        True -> True
        False -> error("exp : "++ show(exprToType [e]) ++ "\n" ++ show(typ))--foldl(\truth x -> checkMbval x) True exp --error("line 105 : " ++ show((checkMbval e) && (checkSameMexpr exp typ)))--foldl(\truth x -> checkMbval x) True exp
    M_real -> error ("line 106 : "++ show((checkMrval e) && (checkSameMexpr exp typ)))--foldl(\trush x -> checkMrval x) True exp
    


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
