package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.service.store.RiskyStore;
import com.spacetimecat.java.lang.unexceptional.Right;
import com.spacetimecat.java.lang.unexceptional.Risky;

final class NullStore implements RiskyStore
{
    static final NullStore instance = new NullStore();

    private NullStore () {}

    @Override
    public Risky<Boolean> add (String name)
    {
        return new Right<>(false);
    }

    @Override
    public Risky<Boolean> remove (String name)
    {
        return new Right<>(false);
    }
}
