package com.spacetimecat.java.prolog;

/**
 * <p>
 * Each instance of this class represents a call to a Prolog predicate.
 * </p>
 * <p>
 * To instantiate this class, call the static factory methods in {@link Calls}.
 * </p>
 */
public abstract class Call {

    private final Unification u = new Unification();

    protected final boolean unify (Term a, Term b) {
        return u.unify(a, b);
    }

    private void undo () {
        u.undo();
    }

    // TODO Make protected "do_reset" and "do_next", and final "reset" and "next",
    // so that it is impossible for implementors to forget to call super.

    /**
     * Remember to call super.reset() at the end of your overriding method.
     */
    public void reset () {
        undo();
    }

    /**
     * Remember to call super.next() first.
     *
     * Find the next successful unification.
     *
     * Return true when the next successful unification has been found.
     * Return false if exhausted (no more possible successes).
     */
    public boolean next () {
        undo();
        return true;
    }

}
