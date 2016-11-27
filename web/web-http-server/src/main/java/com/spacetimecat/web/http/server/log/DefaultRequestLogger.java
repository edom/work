package com.spacetimecat.web.http.server.log;

import com.spacetimecat.web.http.syntax.HttpRequest;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.util.Locale;

public final class DefaultRequestLogger implements RequestLogger
{
    private final OutputStream target;

    public DefaultRequestLogger (OutputStream target)
    {
        this.target = target;
    }

    @Override
    public void log (LogEntry entry)
    {
        final HttpRequest request = entry.getRequest();
        final DateTimeFormatter formatter = DateTimeFormatter.ISO_INSTANT;
        final TemporalAccessor instant = Instant.now();
        final String now = formatter.format(instant);
        final String message =
            String.format(Locale.ENGLISH, "%s %s %s %s %s\n"
                , now
                , entry.getClient()
                , request.getMethod()
                , request.getUri()
                , request.getVersion()
            );
        try
        {
            target.write(message.getBytes(StandardCharsets.UTF_8));
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
}
