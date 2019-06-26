package com.spacetimecat.java.prolog;

final class Call_true extends Call_A {

    private boolean done = false;

    @Override
    protected void do_reset () {
        done = false;
    }

    @Override
    protected boolean do_next () {
        if (done) {
            return false;
        }
        done = true;
        return true;
    }

}
