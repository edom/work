package com.spacetimecat.concurrent.gate;

/**
 * <p>
 *     {@link Gate} providing no mutual exclusion.
 * </p>
 */
public final class Null implements Gate
{
    /**
     * <p>
     *     Run the action.
     * </p>
     *
     * @param action
     * the critical section
     *
     * @return
     * always true
     */
    @Override
    public boolean run (Runnable action)
    {
        action.run();
        return true;
    }
}
