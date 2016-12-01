package com.spacetimecat.concurrent.lock.composite;

import com.spacetimecat.concurrent.lock.Lock;

/**
 * <p>
 *     {@link Lock} that does nothing.
 * </p>
 */
public final class Zero implements Lock
{
    /**
     * <p>
     *     Always return true.
     * </p>
     * @return true
     */
    @Override
    public boolean acquire ()
    {
        return true;
    }

    /**
     * <p>
     *     Do nothing.
     * </p>
     */
    @Override
    public void release ()
    {
    }
}
