package com.spacetimecat;

import java.util.ArrayList;
import java.util.List;

final class LimboImpl implements Limbo
{
    private final List<AutoCloseable> rs = new ArrayList<>();

    @Override
    public Limbo add (AutoCloseable resource)
    {
        rs.add(resource);
        return this;
    }

    public <B extends AutoCloseable> B register (B resource)
    {
        rs.add(resource);
        return resource;
    }

    @Override
    public <A> A closeAndThrow (Throwable cause)
    {
        if (cause == null) { throw new NullPointerException(); }
        for (int i = rs.size() - 1; i >= 0; --i)
        {
            final AutoCloseable r = rs.get(i);
            try { r.close(); }
            catch (Throwable e) { cause.addSuppressed(e); }
        }
        if (cause instanceof RuntimeException) { throw (RuntimeException) cause; }
        if (cause instanceof Error) { throw (Error) cause; }
        throw new UncheckedException(cause);
    }

    @Override
    public void close ()
    {
        Throwable e = null;
        for (int i = rs.size() - 1; i >= 0; --i)
        {
            final AutoCloseable r = rs.get(i);
            try { r.close(); }
            catch (Throwable t)
            {
                if (e == null) { e = t; }
                else { e.addSuppressed(t); }
            }
        }
        if (e == null) { return; }
        if (e instanceof RuntimeException) { throw (RuntimeException) e; }
        if (e instanceof Error) { throw (Error) e; }
        throw new UncheckedException(e);
    }
}
