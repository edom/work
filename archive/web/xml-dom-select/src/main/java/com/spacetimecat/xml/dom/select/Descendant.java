package com.spacetimecat.xml.dom.select;

import com.spacetimecat.xml.dom.Node;

import java.util.function.Function;
import java.util.stream.Stream;

public final class Descendant implements Function<Select, Select>
{
    @Override
    public Select apply (Select that)
    {
        return new Select(doApply(that.unwrap()));
    }

    private Stream<Node> doApply (Stream<Node> that)
    {
        return that.flatMap(n ->
            n.getChildren().flatMap(c ->
                Stream.concat(Stream.of(c), Descendant.this.doApply(Stream.of(c)))
            )
        );
    }
}
