package com.spacetimecat.java.prolog;

final class Term_Var extends Term {

    Term referent;

    //  We cannot simply make a "previous" field because it does not work with setarg/3.

    @Override
    protected Term dereference (int current, int limit) {
        if (referent == null) {
            return this;
        }
        if (current >= limit) {
            throw new Prolog_Exception("unification depth limit reached: " + limit);
        }
        return referent.dereference(current + 1, limit);
    }

    @Override
    public String toString () {
        return (referent == null)
            ? String.format("Var@%x", System.identityHashCode(this))
            : referent.toString();
    }

}
