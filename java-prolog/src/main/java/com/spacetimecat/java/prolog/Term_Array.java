package com.spacetimecat.java.prolog;

final class Term_Array extends Term {

    Term[] array;

    public Term_Array (Term[] array) {
        this.array = array;
    }

    public static Term_Array of (Term... terms) {
        return new Term_Array(terms);
    }

    @Override
    public String toString () {
        StringBuilder s = new StringBuilder();
        s.append('[');
        for (int i = 0; i < array.length; ++i) {
            if (i > 0) {
                s.append(", ");
            }
            s.append(array[i].toString());
        }
        s.append(']');
        return s.toString();
    }

}
