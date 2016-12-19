package com.spacetimecat.concurrent.batch.example;

import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

public final class FakeDatabase
{
    public List<Response> checkAllotments (List<Request> requests)
    {
        final Random random = new Random();
        try
        {
            System.out.printf("FakeDatabase: Checking the availability of %s allotments...\n", requests.size());
            Thread.sleep(200L);
            return requests.stream().map(req -> new Response(random.nextInt(16))).collect(Collectors.toList());
        }
        catch (InterruptedException ignored)
        {
            return requests.stream().map(req -> new Response(0)).collect(Collectors.toList());
        }
    }
}
