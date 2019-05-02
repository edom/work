package com.spacetimecat.collection;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

final class FreeCallableIterator<A>
    extends FreeIterator<Callable<A>>
    implements CallableIterator<A>
{
    FreeCallableIterator (BasicIterator<Callable<A>> bi)
    {
        super(bi);
    }

    @Override
    public Iterator<A> startWith (ExecutorService es)
    {
        return new FreeIterator<>(new ImplicitFutureIterator<>(mapEager(es::submit)));
    }
}
