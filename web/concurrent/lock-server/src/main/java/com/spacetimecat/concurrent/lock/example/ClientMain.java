package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.service.BasicNamespace;
import com.spacetimecat.concurrent.lock.service.Namespace;
import com.spacetimecat.concurrent.lock.service.store.Store;
import com.spacetimecat.concurrent.lock.service.store.internal.network.smp.StoreClient;

public class ClientMain
{
    public static void main (String[] args) throws InterruptedException
    {
        final Store store = StoreClient.connect("//localhost:8100");
        final Namespace namespace = new LoggingNamespace(new BasicNamespace(store));
        final Lock foobar = namespace.get("foobar");
//        new SlowClient(foobar).run();
        new AggressiveClient(namespace).run();
    }
}
