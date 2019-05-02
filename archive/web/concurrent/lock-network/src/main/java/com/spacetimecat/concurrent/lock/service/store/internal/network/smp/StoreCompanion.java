package com.spacetimecat.concurrent.lock.service.store.internal.network.smp;

import com.spacetimecat.concurrent.lock.service.store.ListableStore;
import com.spacetimecat.server.Guest;
import com.spacetimecat.server.imp.SocketGuest;

import java.io.EOFException;
import java.io.IOException;
import java.util.Collection;

public class StoreCompanion implements Runnable
{
    private final ListableStore delegate;
    private final Guest guest;
    private final Protocol protocol;

    public StoreCompanion (ListableStore delegate, SocketGuest guest)
    {
        this.delegate = delegate;
        this.guest = guest;
        this.protocol = Protocol.onSocket(guest.getSocket());
    }

    @Override
    public void run ()
    {
        try (Guest ignored = this.guest)
        {
            for (;;)
            {
                final int method = protocol.readMethod();
                switch (method)
                {
                    case Protocol.M_LIST:
                    {
                        final Collection<String> collection = delegate.list();
                        final String[] array = collection.toArray(new String[collection.size()]);
                        protocol.writeStringArray(array);
                        break;
                    }
                    case Protocol.M_ADD:
                    {
                        final String[] names = protocol.readStringArray();
                        if (names.length != 1) { throw new UnsupportedOperationException("names.length != 1"); }
                        final boolean result = delegate.add(names[0]);
                        protocol.writeBoolean(result);
                        break;
                    }
                    case Protocol.M_REMOVE:
                    {
                        final String[] names = protocol.readStringArray();
                        if (names.length != 1) { throw new UnsupportedOperationException("names.length != 1"); }
                        final boolean result = delegate.remove(names[0]);
                        protocol.writeBoolean(result);
                        break;
                    }
                    default:
                        throw new RuntimeException(String.format("unknown method %s", method));
                }
                protocol.flush();
            }
        }
        catch (EOFException ignored)
        {
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
}
