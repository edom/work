package com.spacetimecat.test;

import com.spacetimecat.collection.FreeIterable;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

public class Test
{
    public static void main (String[] args)
    {
        FreeIterable.from(1, 2, 3, 4, 5).zip(FreeIterable.from(5, 4, 3, 2, 1), new Function2<Integer, Integer, Integer>()
        {
            @Override
            public Integer call (Integer a, Integer b)
            {
                return a * b;
            }
        }).forEach(new Procedure1<Integer>()
        {
            @Override
            public void call (Integer a)
            {
                System.out.println(a);
            }
        });
    }
}
