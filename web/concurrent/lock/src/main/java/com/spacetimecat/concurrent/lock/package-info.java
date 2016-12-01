/**
 * <h1>Binary semaphore</h1>
 *
 * <p>
 *     Binary semaphore has no notion of ownership.
 * </p>
 *
 * <p>
 *     The {@link com.spacetimecat.concurrent.lock.WithLock} control construct
 *     and the {@link com.spacetimecat.concurrent.lock.CriticalSection} abstraction
 *     are equivalent, but {@link com.spacetimecat.concurrent.lock.CriticalSection}
 *     is a standard computer science term.
 * </p>
 *
 * <pre>
 *     new WithLock(lock).run(action)
 *     // is the same as
 *     new CriticalSection(action).runWith(lock)
 * </pre>
 *
 * <p>
 *     If you need multiple locks,
 *     see {@link com.spacetimecat.concurrent.lock.composite}.
 * </p>
 *
 * <p>
 *     The low level interface is {@link com.spacetimecat.concurrent.lock.Lock}.
 * </p>
 */
package com.spacetimecat.concurrent.lock;
