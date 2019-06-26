package com.spacetimecat.java.prolog;

/**
 * <p>
 * This fakes non-determinism by backtracking.
 * </p>
 * <p>
 * For API consumers:
 * </p>
 * <ul>
 * <li>You are only allowed to call {@link #each(Callback)} or {@link #next()}.</li>
 * <li>You must not call any other method declared by this interface.</li>
 * </ul>
 * <p>
 * For API suppliers:
 * </p>
 * <ul>
 * <li>You must implement {@link #reset()}.</li>
 * <li>You must implement at least one of {@link #each(Callback)} or {@link #next()}.</li>
 * </ul>
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
    default boolean next () {
        boolean[] a = new boolean[1];
        a[0] = false;
        each(() -> {
            a[0] = true;
            return Callback.Action.STOP;
        });
        return a[0];
    }

    @FunctionalInterface
    public interface Callback {

        enum Action {
            REDO,
            STOP,
        }

        Callback.Action call ();
    }

    default void each (Callback callback) {
        while (next()) {
            switch (callback.call()) {
                case REDO:
                    break;
                case STOP:
                    return;
                default:
                    throw new AssertionError();
            }
        }
    }

}
