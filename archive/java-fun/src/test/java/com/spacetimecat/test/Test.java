package com.spacetimecat.test;

import com.spacetimecat.collection.Iterable;
import com.spacetimecat.collection.Iterables;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Test
{
    public static void main (String[] args)
    {
        Iterables.from(1, 2, 3, 4, 5)
            .zip(Iterables.from(5, 4, 3, 2, 1), (a, b) -> a * b)
            .forEach(System.out::println);

        {
            final ExecutorService es = Executors.newFixedThreadPool(8);
            final Iterable<Integer> i = Iterables.from(0, 1, 2, 3, 4, 5, 6, 7, 8);
            i.iterator()
                .mapToCallable(a -> () -> Thread.currentThread().getName() + " " + a)
                .startWith(es)
                .forEach(System.out::println);
            es.shutdown();
            System.out.println(i.iterator().all(a -> a >= 3));
            System.out.println(i.iterator().all(a -> a <= 9));
            System.out.println(i.iterator().any(a -> a >= 3));
            System.out.println(i.iterator().any(a -> a <= 9));
            System.out.println(i.iterator().any(a -> a < 0));
        }

        {
            final Iterable<Integer> ii = Iterables.from(0, 1, 2).flatMap(i -> Iterables.from(i, i, i));
            ii.forEach(System.out::println);
            ii.forEach(System.out::println);
        }
    }
}
