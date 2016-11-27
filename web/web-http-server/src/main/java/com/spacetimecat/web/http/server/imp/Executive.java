package com.spacetimecat.web.http.server.imp;

import com.spacetimecat.web.http.io.EndOfInputException;
import com.spacetimecat.web.http.io.HttpInput;
import com.spacetimecat.web.http.io.HttpOutput;
import com.spacetimecat.web.http.server.Guest;
import com.spacetimecat.web.http.server.function.HttpFunction;
import com.spacetimecat.web.http.server.log.DefaultRequestLogger;
import com.spacetimecat.web.http.server.log.LogEntry;
import com.spacetimecat.web.http.server.log.RequestLogger;
import com.spacetimecat.web.http.syntax.HttpRequest;
import com.spacetimecat.web.http.syntax.HttpResponse;
import com.spacetimecat.web.io.BufferedInputStream2;
import com.spacetimecat.web.io.BufferedOutputStream2;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * <p>
 *     One instance of this serves one guest.
 * </p>
 */
public final class Executive implements Runnable
{
    private final Guest guest;
    private final HttpFunction function;

    public Executive (Guest guest, HttpFunction function)
    {
        this.guest = guest;
        this.function = function;
    }

    @Override
    public void run ()
    {
        final RequestLogger requestLogger = new DefaultRequestLogger(System.out);
        try (Guest guest = this.guest)
        {
            final InputStream inputStream = new BufferedInputStream2(guest.getInputStream(), 4096);
            final OutputStream outputStream = new BufferedOutputStream2(guest.getOutputStream(), 4096);
            final HttpInput input = new HttpInput(4096, inputStream);
            final HttpOutput output = new HttpOutput(outputStream);
            for (;;)
            {
                final HttpRequest request = input.readRequest();
                requestLogger.log(new LogEntry(guest.getName(), request));
                final HttpResponse response = function.handle(request);
                output.write(response);
                output.flush();
            }
        }
        catch (EndOfInputException ignored)
        {
        }
        catch (Throwable e)
        {
            e.printStackTrace();
        }
    }
}
