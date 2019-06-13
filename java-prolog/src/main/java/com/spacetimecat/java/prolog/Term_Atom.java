package com.spacetimecat.java.prolog;

final class Term_Atom extends Term {

    private final String string;

    public Term_Atom (String string) {
        this.string = string;
    }

    @Override
    public int hashCode () {
        return string.hashCode();
    }

    @Override
    public boolean equals (Object that_) {
        if (!(that_ instanceof Term_Atom)) {
            return false;
        }
        if (this == that_) {
            return true;
        }
        Term_Atom that = (Term_Atom) that_;
        return this.string.equals(that.string);
    }

    @Override
    public String toString () {
        return string;
    }

    @Override
    public Object to_java_object () {
        return string;
    }

}
