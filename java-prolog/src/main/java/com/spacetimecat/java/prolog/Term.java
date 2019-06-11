package com.spacetimecat.java.prolog;

public class Term {

    Term () {
    }

    public void set_if_Var (Term referent) {
    }

    public Term dereference () {
        return this;
    }

    public boolean is_Var () {
        return false;
    }

    public Var as_Var_or_null () {
        return null;
    }

}
