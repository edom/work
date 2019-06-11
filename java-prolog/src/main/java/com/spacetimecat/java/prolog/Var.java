package com.spacetimecat.java.prolog;

public final class Var extends Term {

    Term referent;

    //  We cannot simply make a "previous" field because it does not work with setarg/3.

    @Override
    public void set_if_Var (Term referent) {
        this.referent = referent;
    }

    @Override
    public Term dereference () {
        if (referent == null) {
            return this;
        }
        return referent.dereference();
    }

    @Override
    public boolean is_Var () {
        return true;
    }

    @Override
    public Var as_Var_or_null () {
        return this;
    }

}
