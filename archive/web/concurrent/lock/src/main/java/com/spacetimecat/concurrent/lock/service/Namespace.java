package com.spacetimecat.concurrent.lock.service;

import com.spacetimecat.concurrent.lock.Lock;

/**
 * <p>
 *     Collection of named locks.
 * </p>
 * <p>
 *     If two resources come from the same namespace and have the same name,
 *     they share the same lock.
 * </p>
 */
public interface Namespace
{
    Lock get (String name);
}
