package com.spacetimecat.java.prolog2;

public final class Compound {

    Object name;
    Object[] args;

    public Compound (Object name, Object... args) {
        this.name = name;
        this.args = args;
    }

    public boolean unify (Unification u, Compound that) {
        return u.unify(this.name, that.name)
            && u.unify_array(this.args, that.args);
    }

    @Override
    public String toString () {
        StringBuilder s = new StringBuilder();
        s.append(name).append('(');
        for (int i = 0; i < args.length; ++i) {
            if (i > 0) {
                s.append(", ");
            }
            s.append(args[i]);
        }
        s.append(')');
        return s.toString();
    }

}
