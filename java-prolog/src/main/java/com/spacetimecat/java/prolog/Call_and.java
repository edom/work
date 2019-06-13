package com.spacetimecat.java.prolog;

class Call_and extends Call_A {

    private final Call[] calls;

    public Call_and (Call[] calls) {
        if (calls.length <= 0) {
            throw new IllegalArgumentException();
        }
        this.calls = calls;
    }

    public static Call_and create (Call... calls) {
        return new Call_and(calls);
    }

    private int depth;

    @Override
    protected void do_reset () {
        for (int i = calls.length - 1; i >= 0; --i) {
            calls[i].reset();
        }
        depth = 0;
    }

    @Override
    protected boolean do_next () {
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
