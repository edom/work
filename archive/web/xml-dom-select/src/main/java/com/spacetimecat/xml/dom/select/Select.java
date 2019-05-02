package com.spacetimecat.xml.dom.select;

import com.spacetimecat.xml.dom.Node;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * <p>
 *     This wraps a {@link Stream} of {@link Node}s.
 * </p>
 */
public final class Select
{
    private final Stream<Node> stream;

    /**
     * <p>
     *     A selection consisting of one node.
     * </p>
     *
     * @param root
     * the node
     */
    public Select (Node root)
    {
        this.stream = Stream.of(root);
    }

    /**
     * <p>
     *     A selection consisting of all nodes in the stream.
     * </p>
     *
     * @param stream
     * all the nodes
     */
    public Select (Stream<Node> stream)
    {
        this.stream = stream;
    }

    public Stream<Node> unwrap ()
    {
        return stream;
    }

    public Set<Node> collect ()
    {
        return new LinkedHashSet<>(stream.collect(Collectors.toList()));
    }

    /**
     * <p>
     *     A sub-selection of nodes from this selection
     *     that also satisfy the predicate.
     * </p>
     *
     * @param accept
     * determines whether a node is included in the returned selection
     *
     * @return
     * a selection containing every node that is in
     * this selection and satisfies the predicate
     */
    public Select filter (Predicate<Node> accept)
    {
        return new Select(stream.filter(accept));
    }

    public Select flatMap (Function<Node, Stream<Node>> f)
    {
        return new Select(stream.flatMap(f));
    }

    /**
     * <p>
     *     This allows you to read the code from left to right
     *     like {@code a.then(b).then(c)}
     *     instead of from right to left like {@code c(b(a))}.
     * </p>
     *
     * @param next
     * function to be applied to this selection
     *
     * @return
     * the result of applying the function to this selection
     */
    public Select then (Function<Select, Select> next)
    {
        return next.apply(this);
    }
}
