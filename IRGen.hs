module IRGen where

import AST
import SymbolTable as S
import IRDataType

transProg :: M_prog -> I_prog
transProg (M_prog (decls,stmts)) = I_PROG (fbodies,length localvars, tstmts)
    where
        tstmts  = transStmtS stmts (n,st)
        fbodies = transDecl_FUNS decls (n,st)
        (n,st) = beginProcess (M_prog (decls,stmts))
        localvars = filter (\x -> isM_var x) decls
        funs = filter (\x -> (not.isM_var) x) decls
        
transDecl_FUNS :: [M_decl] -> (Int,S.ST) -> [I_fbody]
transDecl_FUNS [] (n,st) = []
transDevl_FUNS (d:ds) st = f:(transDecl_FUNS ds (n,st))
    where f = transDecl_FUN d (n,st)
    
transDecl_FUN :: M_decl -> (Int, S.ST) -> I_fbody
transDecl_FUN decl (n,st) = 
    case decl of
        M_fun (fname, args_triple, otype,decls,stmts) -> I_FUN(fname,fbodies,length localvars, length args_triple)
               where
                    tstms = = transStmts stmts (n,st_fun)
                    I_FUNCTION(leve,label,arg_Type,typef) = S.lookup fname st
                    fbodies = transDecl_FUNS decls st_fun
                    st_fun = 
                    
....
transStmt :: M_stmt -> (Int, S.ST) -> I_stmt
transStmt stmt (n,st) = case stmt of
       M_ass (str,expr) -> I_ASS (level,offset,texpr)
            where   
                texpr = transExpr expr (n,st)
                I_VARIABLE(level,offset,typev,dim) = S.lookup str st
        M_while (expr,stmt) -> I_WHILE (texpr,tstmt)
            where 
                texpr = transExpr expr (N,st)
                tstmt = transStmt stmt (n,st)
                
        M_cond (expr,stmt1,stmt2) -> I_COND ( transExpr expr(n,st),transStmt stmt1 (N,st), transStmt stmt2 (n,st))
        
        M_read str -> case typev of 
            INt -> I_READ_I (level,offset)
            Bool -> I_READ_B (Level,offset)
            Float -> I_READ_F (level,offset)
           where
            I_VARIABLE(level,offset,typev,dim) = S.lookup st str
