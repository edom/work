package com.spacetimecat.java.prolog;

final class Call_not extends Call_A {

    private final Call call;

    public Call_not (Call call) {
        this.call = call;
    }

    private boolean done = false;

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
        return !call.next();
    }

}
