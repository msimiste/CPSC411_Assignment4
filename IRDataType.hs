module IRDataType where

data I_prog  = IPROG    ([I_fbody],Int,[I_stmt])
    --    a program node consists of 
    --   (a) the list of functions declared
    --   (b) the number of local variables
    --   (c) the body: a list of statements
    
data I_fbody = IFUN     (string,[I_fbody],Int,Int[I_stmt])
    --    a function node consists of 
    --   (a) the label given to the function
    --   (b) the list of local functions declared
    --   (c) the number of local variables
    --   (d) the number of arguments
    --   (e) the body: a list of statements

data I_stmt = IASS      (Int,Int,I_expr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | iREAD_I   (Int,Int)
            | iPRINT_I  I_expr
            | iREAD_B   (Int,Int)
            | iPRINT_B  I_expr
            | iRETURN   I_expr
            | iBLOCK    ([I_fbody],Int,[I_stmt])
         -- a block consists of 
		 -- (a) a list of local functions
		 -- (b) the number of local varibles declared
		 -- (d) the body: a lst of statements
         
data I_expr = INUM      Int
            | IBOOL     Bool
            | IID       (Int,Int)
            | IAPP      (I_opn,[I_expr])
         --   isize(<level>,<offset>,<which dimension>)
		 --   level and offset identify which array the last integer 
		 --   tells you which dimension you want to look at!!
         
data I_expr = INUM      Int
            | IBOOL     Bool
            | IID       (Int,Int)
            | IAPP      (I_opn,[I_expr])
            
data I_opn = ICALL      (String,Int)
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR;