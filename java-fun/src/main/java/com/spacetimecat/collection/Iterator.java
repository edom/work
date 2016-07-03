package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicFunction2;
import com.spacetimecat.function.BasicPredicate1;
import com.spacetimecat.function.BasicProcedure1;

import java.util.concurrent.Callable;

/**
 * <p>The most likely interface to be heavily used by users of this package.</p>
 *
 * <p>An iterator can be thought as a generator.</p>
 *
 * <p>An iterator produces a sequence of elements.</p>
 *
 * <p>An iterator hides the details of finding the next element.</p>
 *
 * <p>If you have a {@link BasicIterator}, you can get this for free by using {@link Iterators#from(BasicIterator)}.</p>
 *
 * <p>An implementation must satisfy {@code fold(e, f) = scan(e, f).last()}.</p>
 *
 * <p>The closest thing to this in the Java standard library is {@link java.util.stream.Stream}.</p>
 *
 * @see Iterable
 * @see java.util.stream.Stream
 *
 * @param <A> element type
 */
public interface Iterator<A> extends
    BasicIterator<A>
    , Dumpable<A>
    , ToNewStdList<A>
    , Filterable<A>
    , Foldable<A>
    , IntegerIndexed<A>
    , Mappable
    , Scan<A>
{
    boolean all (BasicPredicate1<A> p);

    boolean any (BasicPredicate1<A> p);

    /**
     * <p>A view where this and that are concatenated.</p>
     * @param that the elements that will come after this iterator is exhausted
     * @return a view
     */
    Iterator<A> append (BasicIterator<A> that);

    /**
     * <p>Suppress duplicate elements.</p>
     * @return a view where each element occurs only once;
     * equality is determined by {@link java.util.HashSet}
     */
    Iterator<A> distinct ();

    /**
     * <p>This calls {@link #next()} n times.</p>
     *
     * @param n the number of elements to skip
     *
     * @return this
     */
    Iterator<A> skip (int n);

    /**
     * <p>Call the function for each
     * remaining element in this iterator.</p>
     *
     * @param f procedure taking an element
     *
     * @return this, although it might be useless because this method exhausts this iterator
     */
    Iterator<A> forEach (BasicProcedure1<A> f);

    /**
     * <p>A view of what would be if f were applied to each remaining element.</p>
     * <p>This does not call f immediately.</p>
     * @param f mapping function
     * @param <B> result element type
     * @return a view
     */
    <B> Iterator<B> map (BasicFunction1<A, B> f);

    <B, C> Iterator<C> zip (BasicIterator<B> that, BasicFunction2<A, B, C> f);

    @Override
    Iterator<A> filter (BasicPredicate1<A> p);

    @Override
    Iterator<A> dumpTo (java.util.Collection<A> target);

    @Override
    <B> Iterator<B> scan (B e, BasicFunction2<B, A, B> f);

    /**
     * <p>Get the last element in this iterator.</p>
     *
     * <p>This method exhausts this iterator.</p>
     *
     * @return the last element, or null if there are no more elements
     *
     * @see #next()
     */
    A last ();

    /**
     * <p>Apply f to each remaining element.</p>
     * <p>This allocates a storage that holds those results,
     * and returns a view of that storage.</p>
     * <p>If this iterator does not end,
     * this method should eventually throw an {@link OutOfMemoryError}.</p>
     * @param f mapping function
     * @param <B> result element type
     * @return a view
     */
    <B> Iterator<B> mapEager (BasicFunction1<A, B> f);

    /**
     * <p>This is like {@link #mapEager(BasicFunction1)},
     * but this prepares a parallel execution of {@link Callable}s.</p>
     *
     * <p>The callable must not produce null.</p>
     *
     * @param f describes a computation to do to each element in this iterator
     * @param <B> cannot be {@link Void}; use {@link com.spacetimecat.Null} for that
     *
     * @return a view
     *
     * @see com.spacetimecat.Null
     * @see java.util.Optional
     */
    <B> CallableIterator<B> mapToCallable (BasicFunction1<A, Callable<B>> f);
}
