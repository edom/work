package com.spacetimecat.objmap;

public interface BasicPack<P, A>
{
    /**
     * <p>Serialize an {@code A} to a {@code P}.</p>
     * @param a packed representation
     * @return unpacked representation
     */
    P pack (A a);
}
