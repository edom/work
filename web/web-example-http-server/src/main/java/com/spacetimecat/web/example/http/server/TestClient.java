package com.spacetimecat.web.example.http.server;

import com.spacetimecat.web.http.io.HttpInput;
import com.spacetimecat.web.http.io.HttpOutput;
import com.spacetimecat.web.http.syntax.HttpRequest;
import com.spacetimecat.web.io.BufferedInputStream2;
import com.spacetimecat.web.io.BufferedOutputStream2;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

public final class TestClient
{
    private TestClient () {}

    public static void main (String[] args) throws IOException
    {
        final Socket socket = new Socket("localhost", 8080);
        final InputStream inputStream = new BufferedInputStream2(socket.getInputStream(), 4096);
        final HttpInput input = new HttpInput(4096, inputStream);
        final OutputStream outputStream = new BufferedOutputStream2(socket.getOutputStream(), 4096);
        final HttpOutput output = new HttpOutput(outputStream);
        final HttpRequest request = HttpRequest.get("/", "HTTP/1.1");
        final int n = 16384;
        for (int i = 0; i < n; ++i)
        {
            System.out.println(i);
            output.write(request);
            output.flush();
            input.readResponse();
        }
        socket.close();
    }
}
