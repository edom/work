package com.spacetimecat.join.hash;

import com.spacetimecat.java.lang.function.Function1;
import com.spacetimecat.java.lang.function.Function2;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * <p>
 *     Left outer equi-join using hash table.
 * </p>
 *
 * <p>
 *     See the {@linkplain #HashLeftEquiJoin2(Function1, Function1, Function2) constructor} for more information.
 * </p>
 *
 * @param <K> key type
 * @param <A> left element type
 * @param <B> right element type
 * @param <Z> result type
 */
public final class HashLeftEquiJoin2<K, A, B, Z>
implements Function2<Collection<A>, Collection<B>, Collection<Z>>
{
    private final Function1<A, K> key0;
    private final Function1<B, K> key1;
    private final Function2<A, B, Z> join;

    /**
     * <p>
     *     Left outer equi-join using hash table.
     * </p>
     *
     * <p>
     *     The key type {@code K} must implement
     *     {@link Object#equals(Object) equals} and {@link Object#hashCode()} properly.
     * </p>
     *
     * @param key0
     * should not return null
     *
     * @param key1
     * should not accept null, should not return null
     *
     * @param join
     * must accept null; may return null
     */
    public HashLeftEquiJoin2 (Function1<A, K> key0, Function1<B, K> key1, Function2<A, B, Z> join)
    {
        this.key0 = key0;
        this.key1 = key1;
        this.join = join;
    }

    /**
     * <p>
     *     Performs left outer equi-join on the collections.
     * </p>
     *
     * <p>
     *     Both collections' {@link Collection#size() size} method must have constant time complexity.
     * </p>
     *
     * @param as
     * the left collection
     *
     * @param bs
     * the right collection
     *
     * @return
     * will have null if there is an unmatched left element.
     */
    @Override
    public Collection<Z> apply (Collection<A> as, Collection<B> bs)
    {
        final int size0 = as.size();
        final int size1 = bs.size();
        final int resultSize = Math.max(size0, size1);
        final Collection<Z> result = new ArrayList<>(resultSize);
        final Map<K, B> map1 = new HashMap<>(2 * size0, 0.75F);
        for (B b : bs)
        {
            final K k = key1.apply(b);
            map1.put(k, b);
        }
        for (A a : as)
        {
            final K k = key0.apply(a);
            final B b = map1.get(k);
            final Z z = join.apply(a, b);
            result.add(z);
        }
        return result;
    }
}
