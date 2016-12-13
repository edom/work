package com.spacetimecat.java.lang.resilient;

import com.spacetimecat.java.lang.unexceptional.*;
import com.spacetimecat.java.lang.unit.Unit;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * <p>
 *     A {@code Resilient<T>} is gives a {@code T}
 *     the ability to recover from temporary errors,
 *     given that {@code T} can be instantiated.
 * </p>
 *
 * @param <T> the underlying type
 */
public final class Resilient<T> implements AutoCloseable
{
    private final AtomicReference<T> reference = new AtomicReference<>();
    private final AtomicBoolean weAreResponsible = new AtomicBoolean();
    private final Object closedLock = new Object();
    private boolean closed;

    private final Heaven<T> heaven;

    public Resilient (Heaven<T> heaven)
    {
        this.heaven = heaven;
    }

    /**
     * <p>
     *     Return the available instance immediately,
     *     or return a new instance after some time,
     *     or fail.
     * </p>
     *
     * @return
     * an instance or a failure
     */
    public Risky<T> obtain ()
    {
        {
            final T ref = reference.get();
            if (ref != null) { return new Right<>(ref); }
        }

        if (!weAreResponsible.compareAndSet(false, true))
        {
            final String message = "Another thread is creating a new instance. Try again later.";
            return new Left<>(new TemporaryException(message));
        }

        synchronized (closedLock)
        {
            try
            {
                if (closed)
                {
                    return new Left<>(new ClosedException("This object is already closed."));
                }

                final Risky<T> risky = heaven.create();
                reference.set(risky.getValueOr(null));
                return risky;
            }
            finally
            {
                weAreResponsible.set(false);
            }
        }
    }

    public <R> Risky<R> withInstance (Function<T, Risky<R>> function)
    {
        return obtain().then(
            instance -> function.apply(instance)
                .ifFailRun(() -> kill(instance))
        );
    }

    public <R> Risky<R> withInstanceRisky (FunctionE<T, R> function)
    {
        return withInstance(new RiskyFunction<>(function));
    }

    /**
     * <p>
     *     Call this if the given instance fails.
     * </p>
     *
     * <p>
     *     After this is called,
     *     the next call to {@link #obtain() obtain}
     *     will call the heaven.
     * </p>
     *
     * <p>
     *     It is safe to call this method many times with the same argument.
     * </p>
     *
     * @param instance
     * the failing instance
     *
     * @return
     * whether there is an error
     */
    public Risky<Unit> kill (T instance)
    {
        try
        {
            return heaven.retire(instance);
        }
        finally
        {
            reference.compareAndSet(instance, null);
        }
    }

    /**
     * <p>
     *     This throws what the underlying {@link Heaven#retire(Object) retire} throws.
     * </p>
     */
    @Override
    public void close ()
    {
        synchronized (closedLock)
        {
            if (closed) { return; }
            closed = true;
        }
        final T reference = this.reference.getAndSet(null);
        if (reference != null)
        {
            heaven.retire(reference).throwUnchecked();
        }
    }
}
