package com.spacetimecat.java.lang;

import java.util.concurrent.Callable;

/**
 * <p>
 *     Handle suppressed throwables.
 * </p>
 */
public final class Unchecked
{
    private Throwable original;

    /**
     * <p>
     *     An optional unchecked throwable.
     * </p>
     * @param original
     * can be null
     */
    public Unchecked (Throwable original)
    {
        this.original = original;
    }

    /**
     * <p>
     *     Throw the throwable.
     * </p>
     * <p>
     *     If this is empty, this does nothing.
     * </p>
     * <p>
     *     If the throwable is a checked exception, it is wrapped as the cause of an {@link UncheckedException}.
     * </p>
     */
    public void raise ()
    {
        if (original instanceof RuntimeException) { throw (RuntimeException) original; }
        if (original instanceof Error) { throw (Error) original; }
        if (original != null) { throw new UncheckedException(original); }
    }

    /**
     * <p>
     *     If this is empty, the argument becomes the throwable.
     * </p>
     * <p>
     *     If this already contains a throwable,
     *     this adds the argument as the suppressed throwable
     *     of the original throwable.
     * </p>
     * @param another
     * the argument
     */
    public void add (Throwable another)
    {
        if (original == null) { original = another; }
        else { original.addSuppressed(another); }
    }

    public boolean isEmpty ()
    {
        return original == null;
    }

    /**
     * <p>
     *     Run the action; if the action throws a throwable,
     *     the throwable is {@link #add(Throwable) add}ed to this.
     * </p>
     * @param action
     * what to run
     */
    public void runAndCatch (Runnable action)
    {
        try
        {
            action.run();
        }
        catch (Throwable t)
        {
            add(t);
        }
    }

    public <T> T callAndCatch (Callable<T> action)
    {
        try
        {
            return action.call();
        }
        catch (Throwable t)
        {
            add(t);
            return null;
        }
    }
}
