package com.spacetimecat.java.prolog;

/**
 * Factory methods for creating {@link Term}s.
 */
public final class Terms {

    private Terms () {}

    public static Term atom (String string) {
        return new Term_Atom(string);
    }

    public static Term integer (int value) {
        return Term_Integer.from(value);
    }

    public static Term var () {
        return new Term_Var();
    }

    public static Term array (Term... terms) {
        return Term_Array.of(terms);
    }

    public static Term compound (String name, Term... args) {
        return new Term_Compound(atom(name), args);
    }

    public static Term from_java_object (Object object) {
        if (object == null) {
            throw new NullPointerException();
        }
        if (object instanceof String) {
            return from_java_object((String) object);
        }
        if (object instanceof Integer) {
            return from_java_object(((Integer) object).intValue());
        }
        throw new Prolog_Exception("Cannot convert from " + object.getClass() + " to Term");
    }

    public static Term from_java_object (String string) {
        return atom(string);
    }

    public static Term from_java_object (int value) {
        return integer(value);
    }

}
