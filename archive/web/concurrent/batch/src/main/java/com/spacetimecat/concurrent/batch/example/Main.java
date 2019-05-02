package com.spacetimecat.concurrent.batch.example;

import com.spacetimecat.concurrent.batch.duration.Millisecond;
import com.spacetimecat.concurrent.batch.function.callback.DeduplicatedCallbackFunction;
import com.spacetimecat.concurrent.batch.function.callback.TimeBatchedCallbackFunction;
import com.spacetimecat.concurrent.batch.internal.hash.DefaultEquivalence;
import com.spacetimecat.concurrent.batch.internal.hash.DefaultHashing;
import com.spacetimecat.java.lang.callback.Callbackified;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public final class Main
{
    public static void main (String[] args) throws Exception
    {
        final FakeDatabase db = new FakeDatabase();
        final ScheduledExecutorService executor = Executors.newScheduledThreadPool(64);

        try
        (
            TimeBatchedCallbackFunction<Request, Response> checkAllotments =
                new TimeBatchedCallbackFunction<>
                (
                    executor
                    , new Millisecond(500L)
                    , 64,
                    new DeduplicatedCallbackFunction<>
                    (
                        new DefaultEquivalence<>()
                        , new DefaultHashing<>()
                        , new Callbackified<>(db::checkAllotments)
                    ))
        )
        {
            for (int i = 0; i < 3; ++i)
            {
                final List<Request> requests = generateRequests(16);

                requests.forEach(request ->
                {
                    final boolean accepted = checkAllotments.apply(request, response ->
                    {
                        System.out.printf("%s %s\n", request, response);
                    });

                    if (!accepted)
                    {
                        System.out.printf("%s: Temporarily unavailable.\n", request);
                    }
                });

                Thread.sleep(1000L);
            }
        }
        finally
        {
            executor.shutdown();
        }
    }

    private static Request generateRequest ()
    {
        final Random random = new Random();
        final long thingId = random.nextInt(3);
        final LocalDate date = LocalDate.of(2016, 1, 1 + random.nextInt(3));
        return new Request(thingId, date);
    }

    static List<Request> generateRequests (int count)
    {
        final List<Request> list = new ArrayList<>(count);
        for (int i = 0; i < count; ++i)
        {
            list.add(generateRequest());
        }
        return list;
    }
}
