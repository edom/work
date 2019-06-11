package com.spacetimecat.java.prolog;

public final class Binding {

    final Var var;
    final Term previous;

    public Binding (Var var, Term previous) {
        this.var = var;
        this.previous = previous;
    }

    public void undo () {
        var.referent = previous;
    }

}
