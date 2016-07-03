package com.spacetimecat;

final class LimboFacade implements Limbo
{
    private final Limbo inner;

    LimboFacade (Limbo inner)
    {
        this.inner = inner;
    }

    @Override
    public <A> A closeAndThrow (Throwable t)
    {
        return inner.closeAndThrow(t);
    }

    @Override
    public void close ()
    {
        inner.close();
    }
}
