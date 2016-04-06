
module SymbolTable2 (ST,beginProcess,empty,new_scope,insert,lookup,return)
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
    --M_prog ([],[]) -> []
    M_prog (dec,stm) -> sTable where
        (lastInt,sTable) =  (buildTable 0 tbl rest)
        tbl = new_scope L_PROG empty
        rest = (dec,stm)

--process the rest of the AST
buildTable :: Int -> ST -> ([M_decl],[M_stmt]) -> (Int,ST) 
buildTable n tbl (decl,stm) = (c,sTble) where
    (a,b) = processDecls n tbl decl -- process the [M_decl] ie the list of M_decl
    (c,sTble) = processStmtS a b stm' -- process the {M_stmt] ie the list of M_stmt
    stm' = filter checkStmt stm -- remove the stmt's that are not part of {M_block,M_while,M_cond}   

        
--Helper function, used to verify if an M_stmt belongs to the subset {M_block,M_while,M_cond}        
checkStmt :: M_stmt -> Bool
checkStmt stm = case stm of
    M_while (a,b) -> True
    M_cond (a,b,c) -> True
    M_block (a,b) -> True
    _ -> False  

 {-   
--to process/insert [M_decl] into the Symbol table
processDecls :: Int -> ST -> [M_decl] -> (Int,ST)
processDecls n s [] = (n,s) 
processDecls n s (x:xs) = (n1, tbl) where
    (i,tbl') = insert n1 tbl desc -- insert the M_decl into the Symbol Table
    (desc,(n1,tbl))  = (convertMdec n s x) -- convert the M_decl into a SYM_DESC
    (n2,s1) = processDecls n1 tbl xs   -- Process the rest of [M_decl] the list
-}



processDecls :: Int -> ST -> [M_decl] -> (Int,ST)
processDecls n s [] = (n,s) 
processDecls n s (x:xs) = (n2,s1) where    
    (n1,tbl)  =  processDecl n s x -- convert the M_decl into a SYM_DESC
    (n2,s1) = processDecls n1 tbl xs  -- Process the rest of [M_decl] the list
    
    
    

processDecl :: Int -> ST -> M_decl -> (Int, ST)
processDecl n s x = (num,sTble) where
--processDecl n s x = insert num sTble symdesc where -- remove this 
    (num,sTble) = convertMdec n s x	    

 
--to process/insert [M_stmt] into the Symbol table
processStmtS :: Int -> ST -> [M_stmt] -> (Int, ST)
processStmtS n s [] = (n,s) -- handle an empty list
processStmtS n s (x:xs) = (a,b) where --
    (c,d) = processStmt n s x -- process the first M_stms in the list
    (a,b) = processStmtS c d xs -- process the rest of the list


--process and M_stmt
processStmt :: Int -> ST -> M_stmt -> (Int, ST)
processStmt n s m = case m of    
    --M_block(dec,stm) -> buildTable n s (dec,stm)
    M_block (dec,stm) -> buildTable n (new_scope L_BLK s) (dec,stm) -- add an M_block to the symbol table
    _ -> (n,s)
    --ToDo: add these in for IR version
   -- M_while (expr,stm1) -> 
   -- M_cond (expr,stm1,stm2) ->


--convert an M_decl into a SYM_DESC
convertMdec :: Int -> ST -> M_decl -> (Int,ST)
convertMdec n s x = case x of
    M_var (str,expr,i) -> insert n s (VARIABLE (str,i,(length expr)))
    --(VARIABLE (str,i,(length expr)),(n,s)) --return an M_var converted to a VARIABLE     
    --_ -> ((FUNCTION("mike", [(M_bool,15)], M_real)),(n,s))--,insert n (new_scope (L_FUN M_int) s) (FUNCTION("mike", [(M_bool,15)], M_real)))) 
   -- test = (FUNCTION("mike", [(M_bool,15)], M_real))
    --(test1,test2) = insert n s test
	    
    M_fun func -> insert that another this where
        (this, (that,another)) = processFunction n s func --(str,x,typ,mdec,mstm), process the M_func
    

--to process a function    
processFunction :: Int -> ST -> (String,[(String,Int,M_type)],M_type,[M_decl],[M_stmt]) -> (SYM_DESC,(Int,ST))
processFunction n s (str,list_of_trips,typ,mdec,mstm) =  (symDsc,(cnt,tble)) where 
    tble' = (new_scope (L_FUN typ) s) -- add a new function scope to the symbol table
    symDsc = FUNCTION(str, map strip arGs,typ) -- strips the last 2 argument values ie (String,M_type,Int) => (M_type,Int)
    (arGs,cnt1,tble1) = convertArgs n tble' list_of_trips -- convert all the arguments from (String,Int,M_type) to (String,M_type,Int) and insert them into the Symbol table
    (cnt,tble) = buildTable cnt1 tble1 (mdec,mstm)  -- process the remaining [M_decl] [M_stmt] 




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
