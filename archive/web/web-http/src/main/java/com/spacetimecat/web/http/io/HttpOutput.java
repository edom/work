package com.spacetimecat.web.http.io;

import com.spacetimecat.web.http.syntax.*;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Collection;

/**
 * <p>
 *     Write responses.
 * </p>
 * <p>
 *     Note that the underlying stream may be buffered.
 *     In such case, you need to call {@linkplain #flush() flush}
 *     to really write the things.
 * </p>
 */
public final class HttpOutput
{
    private final OutputStream output;

    public HttpOutput (OutputStream output)
    {
        this.output = output;
    }

    public void flush ()
    {
        try
        {
            output.flush();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    public void write (HttpRequest thing)
    {
        write(thing.header);
    }

    public void write (HttpResponse thing)
    {
        write(thing.header);
    }

    private void write (RequestHeader thing)
    {
        write(thing.requestLine);
        write(thing.headerLines);
        write("\r\n");
    }

    private void write (RequestLine requestLine)
    {
        write(String.format("%s %s %s\r\n", requestLine.method, requestLine.uri, requestLine.version));
    }

    private void write (ResponseHeader thing)
    {
        write(thing.statusLine);
        write(thing.headerLines);
        write("\r\n");
    }

    private void write (StatusLine statusLine)
    {
        write(String.format("%s %s %s\r\n", statusLine.version, statusLine.code, statusLine.reason));
    }

    private void write (Collection<HeaderLine> headerLines)
    {
        for (final HeaderLine headerLine : headerLines)
        {
            write(String.format("%s: %s\r\n", headerLine.name, headerLine.value));
        }
    }

    private void write (String string)
    {
        final byte[] bytes = string.getBytes(StandardCharsets.US_ASCII);
        write(bytes);
    }

    private void write (byte[] bytes)
    {
        try
        {
            output.write(bytes);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
