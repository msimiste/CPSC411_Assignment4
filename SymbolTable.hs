
module SymbolTable (ST,beginProcess,empty,new_scope,insert,lookup,return)
where 
import ST
import AST


empty :: ST 
empty = []
 
--creates a ST with scope as required 
new_scope :: ScopeType -> ST -> ST
new_scope s xs = Symbol_table(s,0,0,[]):xs



--start point where the initial AST is fed into the Symbol table builder
beginProcess :: AST -> ST
beginProcess x = case x of
    M_prog (dec,stm) -> sTable where --sTable 
        (lastInt,sTable) =  (buildTable 0 scope tbl rest)
        tbl = new_scope L_PROG empty
        scope = returnScope tbl
        rest = (dec,stm)

--process the rest of the AST
buildTable :: Int -> ScopeType -> ST -> ([M_decl],[M_stmt]) -> (Int,ST) 
buildTable n scope tbl (decl,stm) = (c,sTble) where
    (a,b) = processDecls n scope tbl decl -- process the [M_decl] ie the list of M_decl
    (c,sTble) = processStmtS a scope b stm' -- process the {M_stmt] ie the list of M_stmt
    stm' = filter checkStmt stm -- remove the stmt's that are not part of {M_block,M_while,M_cond}   

        
--Helper function, used to verify if an M_stmt belongs to the subset {M_block,M_while,M_cond}        
checkStmt :: M_stmt -> Bool
checkStmt stm = case stm of
    M_while (a,b) -> True
    M_cond (a,b,c) -> True
    M_block (a,b) -> True
    _ -> False  


--process the [M_decl] ie the list of M_decl's
processDecls :: Int -> ScopeType -> ST -> [M_decl] -> (Int,ST)
processDecls n scope s [] = (n,s) -- handle the empty list
processDecls n scope s (x:xs) = (n2,s1) where    
    (n1,tbl)  =  processDecl n s x -- convert the M_decl into a SYM_DESC
    (n2,s1) = processDecls n1 scope tbl xs  -- Process the rest of [M_decl] the list    
    
    
--process a single M_decl
processDecl :: Int -> ST -> M_decl -> (Int, ST)
processDecl n s x = (num,sTble) where
    (num,sTble) = convertMdec n s x	    

 
--to process/insert [M_stmt] into the Symbol table
processStmtS :: Int -> ScopeType -> ST -> [M_stmt] -> (Int, ST)
processStmtS n scope s [] = (n,s) -- handle an empty list
processStmtS n scope s (x:xs) = (a,b) where --
    (c,d) = processStmt n s x -- process the first M_stms in the list
    (a,b) = processStmtS c scope d xs -- process the rest of the list


--process an M_stmt
processStmt :: Int -> ST -> M_stmt -> (Int, ST)
processStmt n s m = case m of    
    --M_block(dec,stm) -> buildTable n s (dec,stm)
    M_block (dec,stm) -> (num,s) where
		(num,stble) = buildTable n L_BLK (new_scope L_BLK s) (dec,stm) -- add an M_block to the symbol table
    _ -> (n,s)
    --ToDo: add these in for IR version
   -- M_while (expr,stm1) -> 
   -- M_cond (expr,stm1,stm2) ->


--convert an M_decl into a SYM_DESC
convertMdec :: Int -> ST -> M_decl -> (Int,ST)
convertMdec n s x = case x of
    M_var (str,expr,i) -> insert n s (VARIABLE (str,i,(length expr)))
    M_fun fun ->  processFunction n' s' fun where
        (n',s') = genSymTabFun n (M_fun fun) s
   -- M_fun func -> insert num sTbl symDesc where
     --   (symDesc, (num,sTbl)) = processFunction n s func --(str,x,typ,mdec,mstm), process the M_func
    
--to process a function    
processFunction :: Int -> ST -> (String,[(String,Int,M_type)],M_type,[M_decl],[M_stmt]) -> (Int,ST)
processFunction n s (str,list_of_trips,typ,mdec,mstm) =  (cnt,tble3) 
  where  
    --(n',s') = genSymTabFun n (M_fun(str,list_of_trips,typ,mdec,mstm)) s
    (num3,tble3) = insert n s symDsc -- strips the last 2 argument values ie (String,M_type,Int) => (M_type,Int)
    symDsc = (FUNCTION(str, map strip arGs,typ))
    tble' = (new_scope (L_FUN typ) tble3) -- add a new function scope to the symbol table
    (arGs,cnt1,tble1) = convertArgs num3 tble3 list_of_trips -- convert all the arguments from (String,Int,M_type) to (String,M_type,Int) and insert them into the Symbol table
    
    (cnt,tble) = buildTable cnt1 (returnScope tble1) tble1 (mdec,mstm)
    --(cnt4,tble4) = processDecls cnt1 (returnScope tble1) tble1 (mdec)
    --(cnt,tble) = processStmtS cnt4 (returnScope tble4) tble4 mstm
--converts [M_var] to [ARGUMENT] as required
convertArgs :: Int -> ST -> [(String,Int,M_type)] -> ([SYM_DESC],Int,ST)
convertArgs n s [] = ([], n, s) -- on an empty list return the existing (Int, ST)
convertArgs n s (x:xs) =  ((front:rest),n2,st2) where
    (front,n1,st) = convertArg n s x --convert a single argument from (String,Int,M_type) to (String,M_type,Int) and insert it into the Symbol Table
    (rest,n2,st2) = convertArgs n1 st xs -- convert the rest of the list


--converts and inserts a single M_var to and ARGUMENT
convertArg :: Int -> ST -> (String,Int,M_type) -> (SYM_DESC,Int,ST)
convertArg n s (str,num,typ) = (sym,num1,st) where
    (num1,st) = insert n s sym -- insert the ARGUMENT into the symbol table
    sym = ARGUMENT (str,typ,num) -- convert the (String,Int,M_type) to (String,M_type,Int)


--takes the last 2 values in a tuple and switches their order
strip :: SYM_DESC -> (M_type,Int)
strip (ARGUMENT (a,b,c)) = (b,c) -- remove the first value from a triple 


insert :: Int -> ST -> SYM_DESC -> (Int,ST) --(number of functions, [Symbol_Table])             
insert n [] d = (n, error "Symbol table error: insertion before defining scope.")
insert n ((Symbol_table(scT,nL,nA,sL)):rest) (ARGUMENT(str,t,dim))
    | (in_index_list str sL) = error ("Symbol table error: " ++ str ++"is already defined.")
    | otherwise = (n,(Symbol_table(scT,nL,nA+1,(str,Var_attr(negate (nA+4),t,dim)):sL)):rest)
insert n ((Symbol_table(scT,nL,nA,sL)):rest) (VARIABLE (str,t,dim)) 
    | (in_index_list str sL) =  error ("Symbol table error: "++ str ++"is already defined.")
    | otherwise = (n,(Symbol_table(scT,nL+1,nA,(str,Var_attr(nL+1,t,dim)):sL)):rest)
insert n ((Symbol_table(scT,nL,nA,sL)):rest) (FUNCTION (str,ts,t))
    | in_index_list str sL =  error ("Symbol table error: "++str++"is already defined.")
    | otherwise = (n+1,(Symbol_table(scT,nL,nA,(str,Fun_attr(getlabel n "fn",ts,t)):sL)):rest)


getlabel :: Int -> String -> String
getlabel n s = s ++ "_" ++ show(n)


--helper function -- determines if a value is in a symbol table list
in_index_list :: String -> [(String,SYM_VALUE)] -> Bool  
in_index_list str [] = False
in_index_list str ((x,_):xs)| str==x = True
    | otherwise = in_index_list str xs
    
    
look_up :: ST -> String -> SYM_I_DESC
look_up s x = find 0 s where
    found level (Var_attr(offset,t,dim)) =  I_VARIABLE(level,offset,t,dim)
    found level (Fun_attr(label,arg_Type,t)) = I_FUNCTION(level,label,arg_Type,t)
    found level (Con_attr (cnum, t, name)) = I_CONSTRUCTOR (cnum,t,name)
    found level (Typ_attr s) = I_TYPE s
    
    find n [] = error ("Could not find ")
    find n (Symbol_table(_,_,_,vs):rest) = 
         (case find_level vs of 
            Just v -> found n v
            Nothing -> find (n+1) rest)
            
--find_level :: [(String,SYM_VALUE)] -> Maybe SYM_VALUE
    find_level ((str,v):rest)
        |x == str = Just v
        |otherwise = find_level rest 
    find_level [] = Nothing

genSymTabBlock :: Int -> ScopeType -> [M_decl] -> ST -> (Int, ST)
genSymTabBlock n scope decls st = 
        case scope `elem` [L_PROG,L_BLK] of
            True -> processDecls n scope st' decls
                where
                st' = new_scope scope st
            False -> error "Using wrong symtable"
            
genSymTabFun :: Int -> M_decl -> ST -> (Int, ST)
genSymTabFun n (M_fun (str,args_triple,otype,decls, stmts)) st  = processDecls n' scope st2 decls
    where
        scope = (L_FUN otype)
        (args,a,b) = convertArgs n st args_triple
        st1 = new_scope scope st
        (n',st2) = foldl (\(n',st3) x -> insert n' st3 x) (n,st1) args
        
returnScope :: ST -> ScopeType
returnScope st = case st of
    [Symbol_table(s,_,_,_)] -> s