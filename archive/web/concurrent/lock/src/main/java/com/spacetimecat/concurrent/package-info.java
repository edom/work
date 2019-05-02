/**
 * <h1>Concurrency library</h1>
 *
 * <p>
 *     The message passing style is {@link com.spacetimecat.concurrent.gate.Gate}.
 *     You give it a {@link java.lang.Runnable},
 *     and it takes care of acquiring and releasing the lock.
 * </p>
 *
 * <p>
 *     The service oriented style is {@link com.spacetimecat.concurrent.lock.Lock}.
 *     The user is responsible for acquiring and releasing the lock.
 * </p>
 *
 * <p>
 *     The primitive
 * </p>
 *
 * <h2>Design</h2>
 *
 * <p>
 *     This is rambling.
 * </p>
 *
 * <p>
 *     When objects need to ensure that no more than one of them
 *     is using a shared resource at the same time.
 * </p>
 * <p>
 *     There are many enough objects such that sending a message
 *     to every participating object is not practical.
 * </p>
 * <p>
 *     We introduce a <em>condition variable</em>
 *     that keeps tracks of whether the resource is used.
 * </p>
 * <p>
 *     When a resource cannot be accessed by multiple objects at the same time,
 *     it can implement a protocol that allows cooperating users to regulate access.
 * </p>
 * <p>
 *     A user sends the {@code open} message to the resource.
 *     The resource answers with a boolean whether
 *     the user now owns the resource.
 * </p>
 * <p>
 *     A user sends the {@code close} message to the resource.
 *     The resource answers with a boolean.
 * </p>
 */
package com.spacetimecat.concurrent;
