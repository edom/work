package com.spacetimecat.java.prolog;

import java.util.function.Consumer;

/**
 * <p>
 * Each instance of this class represents a call to a non-deterministic Prolog predicate.
 * Each instance encapsulates the state of a call.
 * </p>
 * <p>
 * A call can be thought of as a sequence of tuples of terms.
 * </p>
 * <p>
 * For API consumers:
 * You are only allowed to call {@link #next()}.
 * </p>
 * <ul>
 * <li>
 * {@link #next()} tries tuples in the search space until the search criterion is satisfied.
 * It returns true if such tuple is found.
 * </li>
 * <li>
 * {@link #undo()} undoes all unifications that are directly made by this call.
 * </li>
 * </ul>
 * <p>
 * For API suppliers:
 * </p>
 * <p>
 * To instantiate this class, call the static factory methods in {@link Calls}.
 * </p>
 */
abstract class Call_A implements Call {

    private final Unification u = new Unification();

    protected final boolean unify (Term a, Term b) {
        return u.unify(a, b);
    }

    /**
     * <p>
     * Undo all unifications that are directly done by this call.
     * </p>
     * <p>
     * Cuts are not undone.
     * </p>
     */
    protected void undo () {
        u.undo();
    }

    /**
     * <p>
     * This undoes unifications, undoes cuts,
     * and restores state to before the first call is done.
     * </p>
     */
    @Override
    public final void reset () {
        //  This is in reverse order with respect to next().
        do_reset();
        undo();
    }

    protected void do_reset () {
    }

    /**
     * Find the next successful unification.
     *
     * Return true when the next successful unification has been found.
     * Return false if exhausted (no more possible successes).
     */
    @Override
    public final boolean next () {
        undo();
        return do_next();
    }

    protected boolean do_next () {
        return false;
    }

    @Override
    public final void each_1 (Term var, Consumer<Object> consumer) {
        while (next()) {
            consumer.accept(var.to_java_object());
        }
    }

}
