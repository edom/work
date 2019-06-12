package com.spacetimecat.java.prolog;

class Call_And extends Call {

    private final Call[] calls;

    public Call_And (Call[] calls) {
        if (calls.length <= 0) {
            throw new IllegalArgumentException();
        }
        this.calls = calls;
    }

    public static Call_And create (Call... calls) {
        return new Call_And(calls);
    }

    private int depth;

    @Override
    public void reset () {
        for (int i = calls.length - 1; i >= 0; ++i) {
            calls[i].reset();
        }
        depth = 0;
        super.reset();
    }

    @Override
    public boolean next () {
        if (!super.next()) {
            return false;
        }
        for (;;) {
            if (depth >= calls.length) {
                --depth;
                return true;
            }
            if (calls[depth].next()) {
                ++depth;
                continue;
            }
            if (depth <= 0) {
                return false;
            }
            calls[depth].reset();
            --depth;
        }
    }

}
