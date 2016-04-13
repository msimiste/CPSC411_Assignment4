module IRGen where
import AST
import SYmbolTable as S
import IRDataType

typeProg :: M_prog -> Bool
typeProg (M_prog (decls,stmts)) = cstmts && cdecls
    where
        cstms = checkStmtS stmst (n, st)
        cdecls = checkDecls decls (n,st)
        (n,st) = getnSymTabBlock 0 L_PROG decls []
    
    
checkDecls :: [M_decl] ->(Int, S.ST) -> Bool
checkDecls [] (n,st) = True
checkDecls (d:ds) sty = bool && (transDecls ds (n,st))
        where bool = cehckDecl d (n,st)


checkDecl :: M_decl -> (Int, S.ST) -> Bool
checkDecl decl sym = case decl of
        M_fun (name, args, outT, decls, stmts) -> bool1 && bool2
            where
                (n,sym2) = genSymTabFun decl sym
                bool1 = checkDecls decls (n,sym2)
                bool2 = checkStmtS stms  (n,sym2)
                
        _ -> True --Other cases are cehcked by symbol tables
      
 checkStmtS :: [M_Stmt] _> (Int, S.ST) -> Bool
 checkStmtS [] (n,st) = True
 checkStmtS (s:ss) (n,st) = bool && (transStmtS ss (n,st))
    where
        bool = checkStmt s (n,st)
        
checkStmt :: M_stmt -> (Int, S.ST) -> Bool
checkStmt stmt sym@(n,st) = case stms of 
        M_ass (s, inds, vl) -> t2 && t1 = t2
            where
                t1 = getExprType (M_id (s,inds))
                t2 = getExprType val st
        M_while (expr, dostmt) -> cond && loop
                where 
        ...
        
        
        
getExprType :: M_Expr -> ST -> M_type
getExprType x st = case x of
    M_ival _ -> M_int
    M_rval _ -> M_real
    M_bval _ -> M_bool
    
    