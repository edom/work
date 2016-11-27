package com.spacetimecat.web.http.server.function;

import com.spacetimecat.web.http.syntax.HttpRequest;
import com.spacetimecat.web.http.syntax.HttpResponse;

public final class IfValid implements HttpFunction
{
    private final HttpFunction then;

    public IfValid (HttpFunction then)
    {
        this.then = then;
    }

    @Override
    public HttpResponse handle (HttpRequest input)
    {
        if (input.isValid())
        {
            return then.handle(input);
        }
        else
        {
            return HttpResponse.badRequest("HTTP/1.1");
        }
    }
}
