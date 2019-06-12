package com.spacetimecat.java.prolog;

/**
 * <p>
 * Each instance of this class represents a Prolog term.
 * </p>
 * <p>
 * To instantiate this class, call the static factory methods in {@link Terms}.
 * </p>
 */
public class Term {

    Term () {
    }

    // -------------------- Var

    private static final int DEFAULT_DEREFERENCE_LIMIT = Integer.MAX_VALUE;

    public final Term dereference () {
        return dereference(DEFAULT_DEREFERENCE_LIMIT);
    }

    /**
     * Return either an unbound Var or a non-Var.
     */
    public final Term dereference (int limit) {
        return dereference(0, limit);
    }

    protected Term dereference (int current, int limit) {
        return this;
    }

}
