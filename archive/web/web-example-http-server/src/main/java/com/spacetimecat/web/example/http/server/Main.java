package com.spacetimecat.web.example.http.server;

import com.spacetimecat.server.Entrance;
import com.spacetimecat.server.Server;
import com.spacetimecat.server.Usher;
import com.spacetimecat.web.http.server.imp.ThreadingHttpUsher;
import com.spacetimecat.server.imp.SocketEntrance;
import com.spacetimecat.web.http.server.function.HttpFunction;
import com.spacetimecat.web.http.server.function.IfValid;
import com.spacetimecat.web.http.server.function.NoContent;

import java.io.IOException;

public final class Main
{
    private Main () {}

    public static void main (String[] args) throws IOException
    {
        final HttpFunction function =
            new IfValid(
                new NoContent()
            );
        final Entrance entrance = SocketEntrance.onPort(8080);
        final Usher usher = ThreadingHttpUsher.create(1024, function);
        new Server(entrance, usher);
    }
}
