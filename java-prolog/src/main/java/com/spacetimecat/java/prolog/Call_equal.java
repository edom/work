package com.spacetimecat.java.prolog;

final class Call_equal extends Call_A {

    private final Term a;
    private final Term b;

    Call_equal (Term a, Term b) {
        this.a = a;
        this.b = b;
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
        done = true;
        return Term.equal(a, b);
    }

}
