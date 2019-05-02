package com.spacetimecat.data;

/**
 * General synchronous key-value store.
 */
public interface KeyValueStore
{
    /**
     * @param key the key to be looked up
     * @return null if the store does not contain the key
     */
    byte[] read (byte[] key);

    void write (byte[] key, byte[] value);
}
