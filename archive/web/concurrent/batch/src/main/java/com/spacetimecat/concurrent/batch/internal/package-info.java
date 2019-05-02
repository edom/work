/**
 * <p>
 *     Low-level implementation details of batching.
 * </p>
 *
 * <p>
 *     The problem: Given a function that is
 *     more efficiently implemented as {@code List a -> List b},
 *     present an interface {@code a -> Consumer b -> ()}
 *     that groups the inputs of the calls that are close in time.
 * </p>
 *
 * <p>
 *     Example of functions that are more efficiently implemented as list functions
 *     are database access functions.
 * </p>
 *
 * <p>
 *     There is also another problem:
 *     Deduplicating the input list.
 *     In this case we want a function
 *     {@code [Call a b] -> Call a b}.
 * </p>
 */
package com.spacetimecat.concurrent.batch.internal;
