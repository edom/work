package com.spacetimecat.concurrent.lock.service.store;

import java.util.Collection;

/**
 * <p>
 *     A {@link Store} that can be listed.
 * </p>
 */
public interface ListableStore extends Store
{
    /**
     * <p>
     *     Return the members of this set.
     * </p>
     * @return the members of this set
     */
    Collection<String> list ();
}
