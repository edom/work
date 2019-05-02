package com.spacetimecat.java.lang.supplier;

import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public final class ListSupplier<A> implements Supplier<List<A>>
{
    private final List<? extends Supplier<A>> suppliers;

    public ListSupplier (List<? extends Supplier<A>> suppliers)
    {
        this.suppliers = suppliers;
    }

    @Override
    public List<A> get ()
    {
        return suppliers.stream().map(Supplier::get).collect(Collectors.toList());
    }
}
