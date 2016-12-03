/**
 * <h1>Binary semaphore</h1>
 *
 * <p>
 *     Binary semaphore has no notion of ownership.
 * </p>
 *
 * <h2>Usage</h2>
 *
 * <p>
 *     The {@link com.spacetimecat.concurrent.lock.WithLock} control construct
 *     and the {@link com.spacetimecat.concurrent.lock.CriticalSection} abstraction
 *     are equivalent, but {@link com.spacetimecat.concurrent.lock.CriticalSection}
 *     is a standard computer science term.
 *     We recommend that you use any of them instead of acquiring and releasing
 *     {@link com.spacetimecat.concurrent.lock.Lock}s manually.
 * </p>
 *
 * <pre>
 *     new WithLock(lock).run(action)
 *     // is the same as
 *     new CriticalSection(action).runWith(lock)
 * </pre>
 *
 * <p>
 *     If you need to acquire multiple locks,
 *     see the {@link com.spacetimecat.concurrent.lock.composite} package.
 * </p>
 *
 * <p>
 *     The low level interface is {@link com.spacetimecat.concurrent.lock.Lock}.
 * </p>
 */
package com.spacetimecat.concurrent.lock;
