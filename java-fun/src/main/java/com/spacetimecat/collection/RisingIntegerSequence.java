package com.spacetimecat.collection;

final class RisingIntegerSequence implements BasicFiniteIterable<Integer>
{
    private final int begin;
    private final int end;
    private final int increment;

    RisingIntegerSequence (int begin, int end, int increment)
    {
        this.begin = begin;
        this.end = end;
        this.increment = increment;
    }

    @Override
    public BasicFiniteIterator<Integer> iterator ()
    {
        return new Iterator_(begin);
    }

    class Iterator_ implements BasicFiniteIterator<Integer>
    {
        private int next;

        Iterator_ (int init)
        {
            this.next = init;
        }

        @Override
        public Integer next ()
        {
            int r = next;
            if (next >= end) { return null; }
            next = r + increment;
            return r;
        }
    }

}
