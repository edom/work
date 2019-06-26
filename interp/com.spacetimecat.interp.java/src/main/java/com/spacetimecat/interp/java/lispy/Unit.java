package com.spacetimecat.interp.java.lispy;

public final class Unit {

    private Unit () {}

    private static final Unit instance = new Unit();

    public static Unit get_instance () {
        return instance;
    }

}
