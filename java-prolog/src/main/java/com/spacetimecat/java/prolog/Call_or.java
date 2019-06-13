package com.spacetimecat.java.prolog;

final class Call_or extends Call_A {

    private final Call[] calls;

    Call_or (Call[] calls) {
        this.calls = calls;
    }

    int current = 0;

    @Override
    protected void do_reset () {
        for (int i = calls.length - 1; i >= 0; --i) {
            calls[i].reset();
        }
        current = 0;
    }

    @Override
    protected boolean do_next () {
        for (;;) {
            if (current >= calls.length) {
                return false;
            }
            if (calls[current].next()) {
                return true;
            }
            ++current;
        }
    }

}
