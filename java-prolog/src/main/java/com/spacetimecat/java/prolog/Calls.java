package com.spacetimecat.java.prolog;

public final class Calls {

    private Calls () {}

    public static Call member (Term elem, Term... list) {
        return new Call_Member(elem, list);
    }

    public static Call and (Call... calls) {
        return new Call_And(calls);
    }

    public static Call or (Call... calls) {
        return new Call_Or(calls);
    }

}
