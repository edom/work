package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.service.store.internal.network.smp.StoreClient;
import com.spacetimecat.java.lang.resilient.Heaven;
import com.spacetimecat.java.lang.unexceptional.Fail;
import com.spacetimecat.java.lang.unexceptional.Ok;
import com.spacetimecat.java.lang.unexceptional.Risky;
import com.spacetimecat.java.lang.unit.Unit;

import java.io.IOException;

final class StoreHeaven implements Heaven<StoreClient>
{
    private final String uri;

    StoreHeaven (String uri)
    {
        this.uri = uri;
    }

    @Override
    public Risky<StoreClient> create ()
    {
        return StoreClient.connect(uri);
    }

    @Override
    public Risky<Unit> retire (StoreClient instance)
    {
        try
        {
            instance.close();
            return new Ok<>(Unit.instance);
        }
        catch (IOException e)
        {
            return new Fail<>(e);
        }
    }
}
