package com.spacetimecat.java.prolog;

final class Call_cut extends Call_A {

    private final Frame frame;

    Call_cut (Frame frame) {
        this.frame = frame;
    }

    boolean done = false;

    @Override
    protected void do_reset () {
        done = false;
    }

    @Override
    protected boolean do_next () {
        if (done) {
            return false;
        }
        frame.cut = true;
        done = true;
        return true;
    }

}
