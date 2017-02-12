package com.spacetimecat.xml.dom;

import org.w3c.dom.NodeList;

import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Node
{
    private final org.w3c.dom.Node n;

    Node (org.w3c.dom.Node n)
    {
        this.n = n;
    }

    public final Stream<Node> getChildren ()
    {
        final NodeList list = n.getChildNodes();
        return IntStream.range(0, list.getLength()).mapToObj(i -> new Node(list.item(i)));
    }

    public org.w3c.dom.Node unwrap ()
    {
        return n;
    }

    @Override
    public String toString ()
    {
        return n.toString();
    }

    @Override
    public boolean equals (Object that)
    {
        if (!(that instanceof Node)) { return false; }
        return this.n.equals(((Node)that).n);
    }

    @Override
    public int hashCode ()
    {
        return n.hashCode();
    }
}
