package com.spacetimecat.java.prolog;

final class Call_once extends Call_A {

    private final Call call;

    Call_once (Call call) {
        this.call = call;
    }

    boolean done = false;

    @Override
    protected void do_reset () {
        done = false;
        call.reset();
    }

    @Override
    protected boolean do_next () {
        if (done) {
            return false;
        }
        done = true;
        return call.next();
    }

}
