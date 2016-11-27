package com.spacetimecat.web.http.server.log;

import com.spacetimecat.web.http.syntax.HttpRequest;

public final class LogEntry
{
    private final String client;
    private final HttpRequest request;

    public LogEntry (String client, HttpRequest request)
    {
        this.client = client;
        this.request = request;
    }

    public String getClient ()
    {
        return client;
    }

    public HttpRequest getRequest ()
    {
        return request;
    }
}
