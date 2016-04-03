
module SymbolTable (ST,something, empty,new_scope,insert,lookup,return)
where 
import ST
import AST

   
empty :: ST 
empty = []
  
new_scope :: ScopeType -> ST -> ST
new_scope s xs = Symbol_table(s,0,0,[]):xs


something :: AST -> ST
something x = case x of
    M_prog ([],[]) -> []
    M_prog (dec,stm) -> snd (buildTable (new_scope L_PROG empty) (dec,stm))


buildTable :: ST -> ([M_decl],[M_stmt]) -> (Int, ST)
buildTable s (dec,stm) = (a,b) where
			templist = (filter checkStmt stm)
			(c,d) = processMlist 0 s dec
			(a,b) = processStmt c d templist



processMlist :: Int -> ST -> [M_decl] -> (Int,ST)
processMlist n s (x:xs) = (out, ts) where
    (out1,ts1) = insert n t sym where
	    (sym,t) = convertMdec s x
    (out,ts) = processMlist out1 ts1 xs
processMlist n s [] = (n,s) 


convertMdec :: ST-> M_decl -> (SYM_DESC,ST)
convertMdec s x = case x of
    M_var (str,typ,i) -> (VARIABLE (str,i,(length typ)),s)
    --M_var (str,typ,i) -> (ARGUMENT (str,i,(length typ)),s)
    M_fun (str,x,typ,mdec,mstm)-> (FUNCTION (str, map strip x ,typ), table) where 
	    (a,table) = buildTable s (mdec, mstm)
	    --[(s,mtyp,i)]= (map strip x)
    _ -> (error "Some kind of error")
    
 {-   
processStmt :: Int -> ST -> [M_stmt] -> (Int, ST)
processStmt n s xs = foldl (\acc x -> processStmt1 n acc x) s xs
--(processStmt1 n s x)++(processStmt(n s xs))


processStmt1 :: Int -> ST -> M_stmt -> (Int, ST)
processStmt1 n s m = case m of
    M_block(dec,stm) -> buildTable s (dec,stm)-}
    
   
processStmt :: Int -> ST -> [M_stmt] -> (Int, ST)
processStmt n s (x:xs) = (a,b) where
    (c,d) = (processStmt1 n s x)
    (a,b) = processStmt c d xs
processStmt n s [] = (n,s)



processStmt1 :: Int -> ST -> M_stmt -> (Int, ST)
processStmt1 n s m = case m of
    M_block(dec,stm) -> buildTable s (dec,stm)

--takes the last 2 values in a tuple and switches their order
strip :: (String,Int,M_type) -> (M_type,Int)
strip (a,b,c) = (c,b)

--inserts a value into the symbol table
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
         (case find_level x vs of 
            Just v -> found n v
            Nothing -> find (n+1) rest)
            
find_level :: String -> [(String,SYM_VALUE)] -> Maybe SYM_VALUE
find_level x ((str,v):rest)
	|x == str = Just v
	|otherwise = find_level x rest 
find_level x [] = Nothing



checkStmt :: M_stmt -> Bool
checkStmt stm = case stm of
    M_while (a,b) -> True
    M_cond (a,b,c) -> True
    M_block (a,b) -> True
    _ -> False
--find :: Int -> ST -> SYM_I_DESC 
     
{-




new_scope :: ScopeType -> ST -> ST
newscope s = (Symbol_table(0,0,[])):s

removeScope :: ScopeType -> ST -> ST

 
return :: ST -> M_type
-}



--lookup:: ST -> string -> SYM_I_DESC
--return:: ST -> M_type
