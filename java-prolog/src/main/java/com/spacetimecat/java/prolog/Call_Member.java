package com.spacetimecat.java.prolog;

final class Call_Member extends Call {

    private final Term elem;
    private final Term[] list;

    public Call_Member (Term elem, Term[] list) {
        this.elem = elem;
        this.list = list;
    }

    public static Call_Member create (Term elem, Term... list) {
        return new Call_Member(elem, list);
    }

    private int current;

    @Override
    public void reset () {
        current = 0;
        super.reset();
    }

    @Override
    public boolean next () {
        if (!super.next()) {
            return false;
        }
        while (current < list.length) {
            if (unify(elem, list[current++])) {
                return true;
            }
        }
        return false;
    }

}
