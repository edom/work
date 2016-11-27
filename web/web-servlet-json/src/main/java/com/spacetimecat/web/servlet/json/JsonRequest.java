package com.spacetimecat.web.servlet.json;

import javax.json.Json;
import javax.json.JsonObject;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

public final class JsonRequest
{
    private final HttpServletRequest request;

    private JsonRequest (HttpServletRequest request)
    {
        this.request = request;
    }

    public static JsonRequest wrap (HttpServletRequest request)
    {
        return new JsonRequest(request);
    }

    public JsonObject readObject () throws IOException
    {
        return Json.createReader(request.getInputStream()).readObject();
    }
}
