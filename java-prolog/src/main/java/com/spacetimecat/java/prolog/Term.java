package com.spacetimecat.java.prolog;

/**
 * <p>
 * Each instance of this class represents a Prolog term.
 * </p>
 * <p>
 * For consumers:
 * To instantiate this class, call the static factory methods in {@link Terms}.
 * </p>
 * <p>
 * For suppliers:
 * Remember these when adding a subclass:
 * </p>
 * <ul>
 * <li>Implement unification for the subclass in {@link Unification#unify(Term, Term)}.</li>
 * <li>Override {@link #to_java_object()}, if it makes sense for the subclass.</li>
 * <li>Override {@link #equals(Object)} and {@link #hashCode()}.</li>
 * <li>Override {@link #toString()}.</li>
 * </ul>
 */
public class Term {

    protected Term () {
    }

    // -------------------- Var

    private static final int DEFAULT_DEREFERENCE_LIMIT = Integer.MAX_VALUE;

    public final Term dereference () {
        return dereference(DEFAULT_DEREFERENCE_LIMIT);
    }

    public Object to_java_object () {
        throw new Prolog_Exception("Cannot convert from " + getClass() + " to Object");
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

    /**
     * For quick testing.
     * Not for writing data for reading back later.
     */
    @Override
    public String toString () {
        return super.toString();
    }

}
