
module SymbolTable (ST,empty,new_scope,insert,lookup,return)
where 
   import ST
   empty:: ST   
   new_scope:: ScopeType -> ST -> ST
   insert:: Int -> ST -> SYM_DESC -> (Int,ST)
   lookup:: ST -> string -> SYM_I_DESC
   return:: ST -> M_type
