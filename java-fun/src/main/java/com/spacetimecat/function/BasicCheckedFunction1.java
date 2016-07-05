package com.spacetimecat.function;

public interface BasicCheckedFunction1<A, B>
{
    B at (A a) throws Throwable;
}
