package com.spacetimecat.concurrent.lock.service.store;

import com.spacetimecat.java.lang.unexceptional.Risky;

public interface RiskyStore
{
    Risky<Boolean> add (String name);

    Risky<Boolean> remove (String name);
}
