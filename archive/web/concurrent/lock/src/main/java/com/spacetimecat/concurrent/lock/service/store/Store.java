package com.spacetimecat.concurrent.lock.service.store;

/**
 * <p>
 *     Mutable set of strings.
 * </p>
 * <p>
 *     If the instance is thread-safe and consistent, then it can be used as
 *     a Lock Service whose 'acquire' is {@linkplain #add(String) add}
 *     and 'release' is {@linkplain #remove(String) remove}.
 *     Every key-value store is mutable set.
 *     Every thread-safe consistent mutable set is a Lock Service.
 * </p>
 */
public interface Store
{
    /**
     * <p>
     *     Add a thing to this set.
     * </p>
     * @param name
     * the thing, cannot be null
     * @return
     * true iff this set changes
     */
    boolean add (String name);

    /**
     * <p>
     *     Remove the thing from this set.
     * </p>
     * @param name
     * the thing, cannot be null
     * @return
     * true iff the set changes
     */
    boolean remove (String name);
}
