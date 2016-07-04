package com.spacetimecat.collection;

import com.spacetimecat.UncheckedException;
import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicProcedure1;

final class FreeRowIterator<A> extends FreeIterator<A> implements RowIterator<A>
{
    private final BasicIteratorFromJdbc<A> bi;

    FreeRowIterator (BasicIteratorFromJdbc<A> bi)
    {
        super(bi);
        this.bi = bi;
    }

    @Override
    public void close () throws UncheckedException
    {
        bi.close();
    }

    @Override
    public RowIterator<A> with (BasicProcedure1<? super RowIterator<A>> f)
    {
        try
        {
            f.call(this);
            return this;
        }
        finally
        {
            close();
        }
    }

    @Override
    public <B> B with (BasicFunction1<? super RowIterator<A>, B> f)
    {
        try
        {
            return f.at(this);
        }
        finally
        {
            close();
        }
    }
}
