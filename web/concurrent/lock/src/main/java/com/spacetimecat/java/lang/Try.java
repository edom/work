package com.spacetimecat.java.lang;

import java.util.Objects;

/**
 * <p>
 *     Try-finally with proper handling of suppressed throwable.
 * </p>
 * <p>
 *     This slightly differs from the built-in try-finally construct.
 *     This may be wrong if an {@link Error}
 *     (especially an {@link StackOverflowError} or an {@link OutOfMemoryError})
 *     is thrown when this tries to execute the finally-part.
 * </p>
 */
public final class Try
{
    private final Runnable tryPart;

    /**
     * @param tryPart
     * the thing to do
     */
    public Try (Runnable tryPart)
    {
        Objects.requireNonNull(tryPart);
        this.tryPart = tryPart;
    }

    /**
     * <p>
     *     Run the try-part, and then run the finally-part regardless of how the try-part returns.
     * </p>
     * <p>
     *     If both the try-part and the finally-part throws a throwable,
     *     then the try-part's throwable is the throwable that will be thrown,
     *     and the finally-part's throwable becomes
     *     a {@linkplain Throwable#getSuppressed() suppressed} throwable of the try-part's throwable.
     * </p>
     * @param finallyPart
     * the thing to do regardless of how the try-part returns
     */
    public void andFinally (Runnable finallyPart)
    {
        Objects.requireNonNull(finallyPart);
        final Unchecked unchecked = new Unchecked(null);
        try
        {
            unchecked.runAndCatch(tryPart);
        }
        finally
        {
            unchecked.runAndCatch(finallyPart);
            unchecked.raise();
        }
    }
}
