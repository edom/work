package com.spacetimecat.java.prolog;

/**
 * <p>
 * An instance of this class represents a search in progress;
 * it contains all the states required to keep track of a search in progress.
 * </p>
 * <p>
 * A search can be thought of as a sequence of tuples of terms.
 * </p>
 * <p>
 * Usage:
 * Call {@link #next()} repeatedly until you find what you want or until it returns {@code false}.
 * Do not call any other method of this class.
 * </p>
 */
public interface Search {

    /**
     * <p>
     * Return {@code true} if something was found just now.
     * </p>
     * <p>
     * Return {@code false} if no more things were found just now, and there are no more results.
     * </p>
     */
    boolean next ();

}
