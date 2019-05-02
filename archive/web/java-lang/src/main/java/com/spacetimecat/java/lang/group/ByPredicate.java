package com.spacetimecat.java.lang.group;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiPredicate;

/**
 * <p>
 *     Computes equivalence classes in {@code O(n^2)} time
 *     where {@code n} is the number of things in the input {@link ArrayList}.
 * </p>
 *
 * <p>
 *     See the {@linkplain #ByPredicate(BiPredicate, List) constructor}
 *     for the time complexity.
 * </p>
 *
 * @param <A>
 * element type
 */
public final class ByPredicate<A> extends GroupThings<A>
{
    private final BiPredicate<A, A> equivalent;

    /**
     * <p>
     *     An object that represents the equivalence classes of the things
     *     according to the equivalence predicate.
     * </p>
     *
     * @param equivalent
     * must be reflexive, symmetric, and transitive
     *
     * @param things
     * the things to be grouped
     */
    public ByPredicate (BiPredicate<A, A> equivalent, List<A> things)
    {
        this.equivalent = equivalent;
    }

    /**
     * <p>
     *     If {@code things} is an {@link ArrayList},
     *     the worst-case time complexity is {@code O(n^2)}
     *     where {@code n} is the length of {@code things}.
     *     The advantage of this is that this only requires a predicate.
     *     If you need a linear-time algorithm,
     *     use a hash table, but then you must provide a function that maps to keys
     *     and you must ensure that you satisfy the hash table's contracts.
     * </p>
     *
     * @param things
     * the things whose equivalence classes are to be computed
     *
     * @return
     * a list of equivalence classes
     */
    @Override
    public List<List<A>> group (List<A> things)
    {
        final List<List<A>> classes = new ArrayList<>();
        List<A> remaining = new ArrayList<>(things);
        List<A> nonMembers = new ArrayList<>(things.size());

        while (!remaining.isEmpty())
        {
            final List<A> members = new ArrayList<>(things.size());

            final A representative = remaining.get(0);

            members.add(representative);

            for (int i = 1; i < remaining.size(); ++i)
            {
                final A thing = remaining.get(i);
                final boolean isMember = equivalent.test(representative, thing);
                (isMember ? members : nonMembers).add(thing);
            }

            classes.add(members);

            {
                final List<A> swap = remaining;
                remaining = nonMembers;
                nonMembers = swap;
                nonMembers.clear();
            }
        }

        return classes;
    }
}
