package com.spacetimecat.objmap;

/**
 * <p>This transforms a packed representation {@code P}
 * to unpacked representation {@code A}.</p>
 */
public interface BasicUnpack<P, A>
{
    /**
     * <p>Deserialize an {@code A} from a {@code P}.</p>
     * @param p packed representation
     * @return unpacked representation
     */
    A unpack (P p);
}
