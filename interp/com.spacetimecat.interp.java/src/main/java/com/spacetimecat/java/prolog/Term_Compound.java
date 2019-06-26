package com.spacetimecat.java.prolog;

import java.util.Arrays;

/**
 * A generalized compound, that is, a compound whose name can be any term
 * instead of being restricted to an atom.
 */
final class Term_Compound extends Term {

    Term name;
    Term[] args;

    public Term_Compound (Term name, Term[] args) {
        this.name = name;
        this.args = args;
    }

    @Override
    public boolean equals (Object that_) {
        if (this == that_) {
            return true;
        }
        if (!(that_ instanceof Term_Compound)) {
            return false;
        }
        Term_Compound that = (Term_Compound) that_;
        return
            this.name.equals(that.name)
         && Arrays.equals(this.args, that.args);
    }

    @Override
    public int hashCode () {
        return 13 * name.hashCode() + Arrays.hashCode(args);
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
