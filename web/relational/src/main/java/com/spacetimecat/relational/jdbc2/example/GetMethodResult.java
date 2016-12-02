package com.spacetimecat.relational.jdbc2.example;

import java.lang.reflect.Method;

final class GetMethodResult extends RuntimeException
{
    final Method method;

    GetMethodResult (Method method)
    {
        super("", null, false, false);
        this.method = method;
    }
}
