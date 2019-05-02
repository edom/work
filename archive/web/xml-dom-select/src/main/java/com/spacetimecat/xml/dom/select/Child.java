package com.spacetimecat.xml.dom.select;

import com.spacetimecat.xml.dom.Node;

import java.util.function.Function;

public final class Child implements Function<Select, Select>
{
    @Override
    public Select apply (Select select)
    {
        return select.flatMap(Node::getChildren);
    }
}
