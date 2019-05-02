package com.spacetimecat.java.lang.function;

@FunctionalInterface
public interface Function2<A, B, C>
{
    C apply (A a, B b);
}
