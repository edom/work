package com.spacetimecat.web.servlet.json;

import com.spacetimecat.web.servlet.Response;

import javax.json.Json;
import javax.json.JsonStructure;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * <p>
 *     A {@link HttpServletResponse} specialized for sending JSON structures.
 * </p>
 */
public final class JsonResponse
{
    private final String mediaType;
    private final HttpServletResponse response;

    private JsonResponse (String mediaType, HttpServletResponse response)
    {
        this.mediaType = mediaType;
        this.response = response;
    }

    public static JsonResponse wrap (HttpServletResponse response)
    {
        return new JsonResponse("application/json", response);
    }

    public void send (JsonStructure structure) throws IOException
    {
        new Response(response).sendOk(mediaType, out -> Json.createWriter(out).write(structure));
    }
}
