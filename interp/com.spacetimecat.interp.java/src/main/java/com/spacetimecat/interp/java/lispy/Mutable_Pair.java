package com.spacetimecat.interp.java.lispy;

public final class Mutable_Pair {

    public Object a;
    public Object b;

    public Place a_place () {
        return new Place() {

            @Override
            public Object get () {
                return a;
            }

            @Override
            public void set (Object value) {
                a = value;
            }
        };
    }

    public Place b_place () {
        return new Place() {

            @Override
            public Object get () {
                return b;
            }

            @Override
            public void set (Object value) {
                b = value;
            }

        };
    }

}
