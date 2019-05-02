package com.spacetimecat.concurrent.batch.example;

import com.spacetimecat.concurrent.batch.duration.Millisecond;
import com.spacetimecat.concurrent.batch.function.callback.CreateMXBean;
import com.spacetimecat.concurrent.batch.function.callback.DeduplicatedCallbackFunction;
import com.spacetimecat.concurrent.batch.function.callback.TimeBatchedCallbackFunction;
import com.spacetimecat.concurrent.batch.internal.hash.DefaultEquivalence;
import com.spacetimecat.concurrent.batch.internal.hash.DefaultHashing;
import com.spacetimecat.java.lang.callback.Callbackified;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import java.lang.management.ManagementFactory;
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public final class MainWithJmx
{
    public static void main (String[] args) throws Exception
    {
        final MBeanServer mbeanServer = ManagementFactory.getPlatformMBeanServer();

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

            {
                final Hashtable<String, String> table = new Hashtable<>();

                table.put("type", "Batcher");
                table.put("class", "FakeDatabase");
                table.put("method", "CheckAllotment");

                mbeanServer.registerMBean(
                    CreateMXBean.forInstance(checkAllotments)
                    , ObjectName.getInstance("com.spacetimecat", table)
                );
            }

            for (;;)
            {
                final List<Request> requests = Main.generateRequests(16);

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
}
