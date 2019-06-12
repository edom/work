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
        Term[] array = new Term[1 + args.length];
        array[0] = atom(name);
        System.arraycopy(args, 0, array, 1, args.length);
        return Term_Array.of(array);
    }

}
