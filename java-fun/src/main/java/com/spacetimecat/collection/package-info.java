/**
 * <p>Collections are immutable by default.
 * Mutable collections have 'Mutable' in their names.</p>
 *
 * <p>If you use this package, you should not use the
 * java.util collection types directly.
 * In particular, if you are using this package,
 * you should never need to write any 'for' loops,
 * unless you are working with arrays.
 * If you have to resort to such measures because this library
 * does not provide an obvious way of doing the same thing,
 * please consider reporting a bug for this package.</p>
 *
 * <p><em>Note about thread safety:</em>
 * Everything is <em>not</em> thread-safe
 * unless explicitly written otherwise.</p>
 *
 * <h2>Common usage</h2>
 *
 * <p>The interface {@link com.spacetimecat.collection.Iterator}
 * is probably the most important interface for the users of this package.</p>
 *
 * <p>The interfaces
 * {@link com.spacetimecat.collection.Iterable}
 * and {@link com.spacetimecat.collection.Iterator}
 * are designed for the convenience of the users of this library.
 * You might not even need to know the other interfaces.</p>
 *
 * <p>To obtain an {@link com.spacetimecat.collection.Iterable},
 * use any static methods of the factory class
 * {@link com.spacetimecat.collection.Iterables}.</p>
 *
 * <h3>Converting from standard Java classes</h3>
 *
 * <p>See {@link com.spacetimecat.collection.Iterators#from(java.util.Iterator)}.</p>
 *
 * <h3>Converting to standard Java classes</h3>
 *
 * <p>See {@link com.spacetimecat.collection.Dumpable#dumpTo(java.util.Collection)}.</p>
 *
 * <p>See {@link com.spacetimecat.collection.ToNewStdList#toNewStdList()}</p>
 *
 * <h2>Implementing your own Iterator</h2>
 *
 * <p>You only need to implement a {@link com.spacetimecat.collection.BasicIterator}.</p>
 *
 * <p>You can then get an {@link com.spacetimecat.collection.Iterator} for free
 * from {@link com.spacetimecat.collection.Iterators#from(com.spacetimecat.collection.BasicIterator)}.</p>
 *
 * <h2>Paradigms</h2>
 *
 * <p>An {@link com.spacetimecat.collection.Iterator} can be seen as a lazy stream.</p>
 *
 * <p>An {@link com.spacetimecat.collection.Iterator} can be seen as a reactive value.</p>
 *
 * <h2>Comparison with other libraries</h2>
 *
 * <p>We think this library is the collection library
 * that most strictly follows
 * the Interface Segregation Principle.</p>
 *
 * <h3>Standard Java 8 {@link java.util.stream}</h3>
 *
 * <p>The standard library provides much more than this library does,
 * but our library is simpler and more understandable.</p>
 *
 * <h3>Guava</h3>
 *
 * <p>Guava <em>extends</em> the standard {@link java.lang.Iterable}.</p>
 *
 * <p>Guava provides much more than collections.</p>
 *
 * <p>This library <em>wraps</em> the standard {@link java.lang.Iterable} in
 * its own {@link com.spacetimecat.collection.Iterable}.</p>
 *
 * <p>We think a Mutable Something should extend an immutable Something
 * instead of the other way around.</p>
 *
 * <h3>Apache Collections</h3>
 *
 * <p>Similar to Guava,
 * Apache Collections extends the types in the Java standard library.</p>
 *
 * <p>We wrap the types in the Java standard library.</p>
 *
 * <p>Apache Collections is more complete than this library,
 * but this library has better naming convention.</p>
 *
 * <h3>Functional Java</h3>
 *
 * <p>Functional Java seems to imagine that Java had Haskell's type system.</p>
 *
 * <p>For example, we think Functional Java's {@code fj.data.IO} interface
 * makes Java code unnecessarily harder to read because
 * Java's type system cannot prevent
 * the compilation of a {@link java.util.List}
 * implementation whose {@link java.util.List#isEmpty()} method
 * writes to a file anyway.</p>
 *
 * <p>We accept the fact that Java is an imperative language.
 * We design this library with usability by Java programmers in mind.</p>
 */
package com.spacetimecat.collection;