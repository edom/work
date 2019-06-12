package com.spacetimecat.java.prolog;

final class Call_Or extends Call {

    private final Call[] calls;

    Call_Or (Call[] calls) {
        this.calls = calls;
    }

    int current = 0;

    @Override
    public void reset () {
        for (int i = calls.length - 1; i >= 0; ++i) {
            calls[i].reset();
        }
        current = 0;
        super.reset();
    }

    @Override
    public boolean next () {
        if (!super.next()) {
            return false;
        }
        for (;;) {
            if (current >= calls.length) {
                return false;
            }
            if (calls[current].next()) {
                return true;
            }
            super.reset();
            ++current;
        }
    }

}
