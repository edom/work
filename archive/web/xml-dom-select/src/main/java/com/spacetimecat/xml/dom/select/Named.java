package com.spacetimecat.xml.dom.select;

import java.util.function.Function;

public final class Named implements Function<Select, Select>
{
    private final String name;

    public Named (String name)
    {
        this.name = name;
    }

    @Override
    public Select apply (Select select)
    {
        return select.filter(n -> name.equals(n.unwrap().getNodeName()));
    }
}
