package com.spacetimecat.java.lang.unexceptional;

public interface FunctionE<A, B>
{
    B apply (A input) throws Exception;
}
