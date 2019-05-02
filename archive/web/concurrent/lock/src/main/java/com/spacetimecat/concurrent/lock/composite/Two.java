package com.spacetimecat.concurrent.lock.composite;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.java.lang.Rollback;
import com.spacetimecat.java.lang.Try;

/**
 * <p>
 *     {@link Lock} made of a sequence of two locks.
 * </p>
 */
public final class Two implements Lock
{
    private final Lock first;
    private final Lock second;

    /**
     * <p>
     *     A lock that will lock {@code first} and then {@code second},
     *     and will release {@code second} and then {@code first}.
     * </p>
     * @param first
     * the first lock that will be acquired, the second lock that will be released
     * @param second
     * the second lock that will be acquired, the first lock that will be released
     */
    public Two (Lock first, Lock second)
    {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean acquire ()
    {
        if (first.acquire())
        {
            try (Rollback rollback = new Rollback(first::release))
            {
                if (second.acquire())
                {
                    rollback.disable();
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void release ()
    {
        new Try(second::release).andFinally(first::release);
    }
}
