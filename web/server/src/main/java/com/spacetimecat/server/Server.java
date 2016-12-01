package com.spacetimecat.server;

public final class Server implements Runnable
{
    private final Entrance entrance;
    private final Usher usher;

    public Server (Entrance entrance, Usher usher)
    {
        this.entrance = entrance;
        this.usher = usher;
    }

    @Override
    public void run ()
    {
        try
            (
                Entrance entrance = this.entrance
                ; Usher usher = this.usher
            )
        {
            for (;;)
            {
                Guest guest = entrance.next();
                usher.handle(guest);
            }
        }
    }
}
