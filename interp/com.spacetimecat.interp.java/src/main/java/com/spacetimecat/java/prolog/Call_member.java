package com.spacetimecat.java.prolog;

final class Call_member extends Call_A {

    private final Term elem;
    private final Term[] list;

    public Call_member (Term elem, Term[] list) {
        this.elem = elem;
        this.list = list;
    }

    public static Call_member create (Term elem, Term... list) {
        return new Call_member(elem, list);
    }

    private int current;

    @Override
    protected void do_reset () {
        current = 0;
    }

    @Override
    protected boolean do_next () {
        while (current < list.length) {
            if (unify(elem, list[current++])) {
                return true;
            }
        }
        return false;
    }

}
