package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.CriticalSection;
import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.composite.Many;
import com.spacetimecat.concurrent.lock.service.Namespace;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

final class AggressiveClient implements Runnable
{
    private final Namespace namespace;

    AggressiveClient (Namespace namespace)
    {
        this.namespace = namespace;
    }

    @Override
    public void run ()
    {
        final Lock lock = Many.of(
            IntStream.range(0, 16)
                .mapToObj(n -> String.format("foo%s", n))
                .map(namespace::get)
                .collect(Collectors.toList())
        );
        new CriticalSection(this::criticalSection).runWith(lock);
    }

    private void criticalSection ()
    {
        System.out.println("hello");
    }
}
