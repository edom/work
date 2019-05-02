package com.spacetimecat.web.http.server.function;

import com.spacetimecat.web.http.syntax.HttpRequest;
import com.spacetimecat.web.http.syntax.HttpResponse;

@FunctionalInterface
public interface HttpFunction
{
    HttpResponse handle (HttpRequest input);
}
