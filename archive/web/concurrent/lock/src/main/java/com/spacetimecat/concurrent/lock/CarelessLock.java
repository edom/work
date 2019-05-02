package com.spacetimecat.concurrent.lock;

/**
 * <p>
 *     A {@link Lock} that wraps a {@link RiskyLock} by silently ignoring errors.
 * </p>
 */
public final class CarelessLock implements Lock
{
    private final RiskyLock delegate;

    public CarelessLock (RiskyLock delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public boolean acquire ()
    {
        return delegate.acquire().getValueOr(false);
    }

    @Override
    public void release ()
    {
        delegate.release();
    }
}
