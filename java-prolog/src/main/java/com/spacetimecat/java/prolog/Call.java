package com.spacetimecat.java.prolog;

import java.util.function.Consumer;

/**
 * <p>
 * For API consumers:
 * You are only allowed to call {@link #next()}.
 * You must not call any other method declared by this interface.
 * </p>
 */
public interface Call extends Search {

    /**
     * <p>
     * This undoes unifications, undoes cuts,
     * and restores state to before the first call is done.
     * </p>
     */
    void reset ();

    /**
     * <p>
     * Find the next successful unification.
     * </p>
     * <p>
     * Return true when the next successful unification has been found.
     * Return false if exhausted (no more possible successes).
     * </p>
     * <p>
     * If this returns false, this must undo partial unifications,
     * and must not leave any partial unifications visible.
     * </p>
     */
    @Override
    boolean next ();

    void each_1 (Term var, Consumer<Object> consumer);

}
