
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
    M_prog ([],[]) -> []
    M_prog (dec,stm) -> snd (buildTable 0 tbl rest) where
        tbl = new_scope L_PROG empty
        rest = (dec,stm)

--process the rest of the AST
buildTable :: Int -> ST -> ([M_decl],[M_stmt]) -> (Int,ST)
buildTable n tbl (decl,stm) = (n,sTble) where
    (a,b) = processDecl n tbl decl -- process the [M_decl] ie the list of M_decl
    (c,sTble) = processStmtS a b stm' where -- process the {M_stmt] ie the list of M_stmt
        stm' = filter checkStmt stm
        
--Helper function, used to verify if an M_stmt belongs to the subset {M_block,M_while,M_cond}        
checkStmt :: M_stmt -> Bool
checkStmt stm = case stm of
    M_while (a,b) -> True
    M_cond (a,b,c) -> True
    M_block (a,b) -> True
    _ -> False  

    
--to process/insert [M_decl] into the Symbol table
processDecl:: Int -> ST -> [M_decl] -> (Int,ST)
processDecl n s (x:xs) = (n, tbl) where
    (i,tbl') = insert conv1 tbl desc -- insert
    (desc,(conv1,tbl))  = (convertMdec i tbl' x)         
processMlist n s [] = (n,s) 


--to process/insert [M_stmt] into the Symbol table
processStmtS :: Int -> ST -> [M_stmt] -> (Int, ST)
processStmtS n s (x:xs) = (a,b) where
    (c,d) = processStmt n s x
    (a,b) = processStmtS c d xs
processStmtS n s [] = (n,s)


processStmt :: Int -> ST -> M_stmt -> (Int, ST)
processStmt n s m = case m of
    M_block(dec,stm) -> buildTable n s (dec,stm)
    --ToDo: add these in for IR version
   -- M_while (expr,stm1) -> 
   -- M_cond (expr,stm1,stm2) ->


convertMdec :: Int -> ST -> M_decl -> (SYM_DESC,(Int,ST))
convertMdec n s x = case x of
    M_var (str,expr,i) -> (VARIABLE (str,i,(length expr)),(n,s))      
    M_fun func -> processFunction n s func --(str,x,typ,mdec,mstm)
    
processFunction :: Int -> ST -> (String,[(String,Int,M_type)],M_type,[M_decl],[M_stmt]) -> (SYM_DESC,(Int,ST))
processFunction n s (str,list_of_trips,typ,mdec,mstm) =  (symDsc,(cnt,tble)) where 
	tble' = (new_scope (L_FUN typ) s)
	symDsc = FUNCTION(str, map strip arGs,typ)
	(arGs,cnt,tble) = convertArgs n tble' list_of_trips
    
    --(FUNCTION (str, map strip x ,typ),(new_scope (L_FUN typ) table)) where 
      --  (a,table) = buildTable s (mdec, mstm)
	    --[(s,mtyp,i)]= (map strip x)
   -- _ -> (error "Some kind of error")
--to process/insert [M_stmt] ino the symbol table

--takes the last 2 values in a tuple and switches their order
switch :: (String,Int,M_type) -> (M_type,Int)
switch (a,b,c) = (c,b)



convertArgs :: Int -> ST -> [(String,Int,M_type)] -> ([SYM_DESC],Int,ST)
convertArgs n s [] = ([], n, s) 
convertArgs n s (x:xs) =  ((front:rest),n2,st2) where
    (front,n1,st) = convertArg n s x
    (rest,n2,st2) = convertArgs n1 st xs

convertArg :: Int -> ST -> (String,Int,M_type) -> (SYM_DESC,Int,ST)
convertArg n s (str,num,typ) = (sym,num,st) where
	(num,st) = insert n s sym
	sym = ARGUMENT (str,typ,num) 

--takes the last 2 values in a tuple and switches their order
strip :: SYM_DESC -> (M_type,Int)
strip (ARGUMENT (a,b,c)) = (b,c)


insert :: Int -> ST -> SYM_DESC -> (Int,ST) --(number of functions, [Symbol_Table])             
insert n [] d = (n, error "Symbol table error: insertion before defining scope.")
insert n ((Symbol_table(scT,nL,nA,sL)):rest) (ARGUMENT(str,t,dim))
    | (in_index_list str sL) = error ("Symbol table error: " ++ str ++"is already defined.")
    | otherwise = (n,[Symbol_table(scT,nL,nA+1,(str,Var_attr(negate (nA+4),t,dim)):sL)])
insert n ((Symbol_table(scT,nL,nA,sL)):rest) (VARIABLE (str,t,dim)) 
    | (in_index_list str sL) =  error ("Symbol table error: "++ str ++"is already defined.")
    | otherwise = (n,[Symbol_table(scT,nL+1,nA,(str,Var_attr(nL+1,t,dim)):sL)])
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
