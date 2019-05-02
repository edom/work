package com.spacetimecat.java.lang.group;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p>
 *     Computes the equivalence classes of {@code n} things of type {@code A}
 *     in amortized {@code O(n)} time using a {@link HashMap},
 *     assuming that the hashing is fast and dispersive enough,
 *     where equivalence is defined by
 *     the {@link Object#hashCode() hashCode}
 *     and {@link Object#equals(Object) equals}
 *     of {@code A}.
 * </p>
 *
 * <p>
 *     To prevent confusion due to dynamic dispatch,
 *     you should use {@code A} only
 *     and not any other subtype of {@code A}.
 * </p>
 *
 * @param <A>
 * element type
 */
public final class ByHash<A> extends GroupThings<A>
{
    @Override
    public List<List<A>> group (List<A> things)
    {
        final int size = things.size();
        final Map<A, List<A>> map = new HashMap<>(2 * size, 0.75F);
        things.forEach(thing ->
            map.compute(thing, (key, val) ->
            {
                if (val == null)
                {
                    val = new ArrayList<>(size);
                }
                val.add(thing);
                return val;
            }));
        return map.values().stream()
            .map(ArrayList::new)
            .collect(Collectors.toList());
    }
}
