
module ST where

import AST

--insertion

data SYM_DESC = ARGUMENT (String,M_type,Int)
              | VARIABLE (String,M_type,Int)
              | FUNCTION (String,[(M_type,Int)],M_type)
              | DATATYPE String 
              | CONSTRUCTOR (String, [M_type], String)
              deriving (Eq,Show,Ord, Read)

data SYM_I_DESC = I_VARIABLE (Int,Int,M_type,Int)
			  | I_FUNCTION (Int,String,[(M_type,Int)],M_type)
			  | I_CONSTRUCTOR (Int,[M_type],String)
			  | I_TYPE [String]
			  deriving (Eq,Show,Ord, Read)
			  
data ScopeType = L_PROG 
              | L_FUN M_type 
              | L_BLK 
              | L_CASE
              deriving (Eq,Show,Ord, Read)
              
data SYM_VALUE = Var_attr (Int,M_type,Int)
              | Fun_attr (String,[(M_type,Int)],M_type)
              | Con_attr (Int, [M_type], String)
              | Typ_attr [String]
              deriving (Eq,Show,Ord, Read)
                


data SYM_TABLE = Symbol_table (ScopeType,Int,Int,[(String,SYM_VALUE)])
    deriving (Eq,Show,Ord, Read)

type ST = [SYM_TABLE]

