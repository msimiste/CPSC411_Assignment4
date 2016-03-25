
module SymbolTable (ST,empty,new_scope,insert,lookup,return)
where 
import ST
   
empty :: ST 
empty = []
  
new_scope:: ScopeType -> ST -> ST
newscope s (st(a,b,[])) = (Symbol_table(a+1,b+1,[])):s

insert:: Int -> ST -> SYM_DESC -> (Int,ST)
insert --ARGUMENT (String,M_type,Int)
insert --              | VARIABLE (String,M_type,Int)
insert --              | FUNCTION (String,[(M_type,Int)],M_type)
insert--              | DATATYPE String 
insert --              | CONSTRUCTOR (String, [M_type], String)



lookup:: ST -> string -> SYM_I_DESC
return:: ST -> M_type
