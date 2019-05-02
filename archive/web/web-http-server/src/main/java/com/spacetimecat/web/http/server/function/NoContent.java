package com.spacetimecat.web.http.server.function;

import com.spacetimecat.web.http.syntax.HttpRequest;
import com.spacetimecat.web.http.syntax.HttpResponse;

public final class NoContent implements HttpFunction
{
    @Override
    public HttpResponse handle (HttpRequest input)
    {
        final String version = input.getVersionOr("HTTP/1.1");
        return HttpResponse.noContent(version);
    }
}
