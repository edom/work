package com.spacetimecat.concurrent.gate;

import java.util.concurrent.Callable;

/**
 * <p>
 *     Allow at most one {@link Runnable} to run at the same time.
 * </p>
 * <p>
 *     The previous runnable must exit before the next runnable can enter.
 * </p>
 * <p>
 *     Worker: do at most one thing.
 * </p>
 * <p>
 *     We could make an {@link java.util.concurrent.Executor} on a {@link Gate},
 *     but failure to obtain the lock is to be expected and is not an exceptional condition,
 *     so we shouldn't adapt a {@link Gate} into an {@link java.util.concurrent.Executor}.
 * </p>
 * <p>
 *     Promising name candidates:
 *     Gate
 *     Worker
 *     SingleWorker
 * </p>
 * <p>
 *     Name candidates:
 *     Bathroom
 *     Lavatory
 *     Restroom
 *     Toilet
 *     Room
 *     Studio
 *     FittingRoom
 *     PrivateSpace
 *     ChokePoint
 *     ConvenientSemaphore
 *     Bottleneck
 *     Mutexer
 *     Excluder
 *     MutualExcluder
 *     Serializer
 *     Sychronizer
 *     DoOneThing
 *     Governor
 *     OneAtATime
 *     OneByOne
 *     Room.occupy
 *     OneSizedQueue
 *     SingletonQueue
 *     Congestion
 *     OneManWay
 *     Slot
 *     ExecutionSlot
 *     Door
 *     TakeTurn
 *     OneThing
 *     RareThing
 *     Hazard
 *     Clog
 *     Neck
 *     NarrowLane
 *     Bridge
 *     Owner
 *     Pass
 * </p>
 * <p>
 *     This is similar to the {@code synchronized} block,
 *     but this does not check the thread.
 * </p>
 * <p>
 *     The mutex is non-recursive.
 *     It is not re-entrant.
 *     Instances do not remember threads.
 *     The owner cannot acquire the mutex again
 *     while it owns the mutex.
 * </p>
 * <pre>
 *     // synchronized style
 *     synchronized (lock) { action... }
 *
 *     // generalized style
 *     mutex.run(() -&gt; { action... });
 * </pre>
 * @deprecated Use {@link com.spacetimecat.concurrent.lock.WithLock}
 */
@Deprecated
public interface Gate
{
    /**
     * <p>
     *     Acquire the lock, run the action, release the lock.
     * </p>
     *
     * <p>
     *     This does not require that the is performed on the calling thread.
     * </p>
     *
     * <p>
     *     After control returns to this (including by throwing),
     *     the mutex is released.
     * </p>
     *
     * <p>
     *     If control does not return to this
     *     (such as because the action crashes the JVM or calls {@link System#exit(int)})
     *     the mutex is not released.
     * </p>
     *
     * <p>
     *     We considered taking {@link Callable}
     *     but it complicated the interface.
     * </p>
     *
     * @param action
     * the critical section that must not be run by multiple threads
     *
     * @return true if the action was run.
     * False means that we couldn't immediately acquire the mutex
     * because another thread was using it
     * and thus the action was not run.
     */
    boolean run (Runnable action);
}
