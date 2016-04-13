generate_sym_decl :: Int -> Scopetype -> M_decl -> ST -> (Int,ST)
generate_sym_decl n scope decl stabs = case of
    M_var (vname,exprs,vtype) -> (n',st')
        where
            sym_Desc = VARIABLE(vname,vtype,length exprs)
            (n',st') = insert n scope stabs sym_Desc
    M_fun (Fname,arges_triple, otype,decls,stmts) ->
            (n',new_st)
        where
            (n',new_st) = insert n scope stabs sym_desc
            sym_desc  = FUNCTION (fname,args_pair,otype)
            args_pair = mape(\(name,dim,typea) -> (typea,dim)) args_triple

genSymTabBlock :: Int -> ScopeType -> [M_decl] -> ST -> (Int, ST)
genSymTabBlock n scope decls st = 
        case scop 'ele' [L_PROG,L_BLK] of
            True -> generate_syym_decls n scope decls st'
                hwere
                st' = new_scope scope st
            False -> error "Using wrong symtable"
            
genSymTabFun :: Int -> M_decl -> St -> (Int, ST)
genSymTabFun n (M_fun (str,args_triple,otype,decls, stmts)) st  = generate_syym_decls n' scope decls st2
    where
        scope = (L_FUN otuype0
        args = add_Argument args_triple
        st1 = new_scope scope st
        (n',st2) = insert list n scope st1 args
        