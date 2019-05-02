package com.spacetimecat.collection;

final class BasicIteratorNanoStopwatch implements BasicIterator<Long>
{
    private long then = System.nanoTime();

    @Override
    public Long next ()
    {
        final long now = System.nanoTime();
        final long elapsed = now - then;
        then = now;
        return elapsed;
    }
}
