package com.spacetimecat.relational.jdbc2.example;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

final class GetMethodInvocationHandler implements InvocationHandler
{
    @Override
    public Object invoke (Object proxy, Method method, Object[] args)
    {
        throw new GetMethodResult(method);
    }

}
