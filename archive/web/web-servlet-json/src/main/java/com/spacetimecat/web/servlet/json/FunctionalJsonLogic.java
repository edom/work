package com.spacetimecat.web.servlet.json;

import com.spacetimecat.web.servlet.logic.Logic;

import javax.json.JsonObject;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * <p>
 *     JSON in, JSON out.
 * </p>
 */
public final class FunctionalJsonLogic implements Logic
{
    /**
     * <p>
     *     JSON in, JSON out.
     * </p>
     */
    public interface Delegate
    {
        JsonObject handle (JsonObject input) throws IOException, ServletException;
    }

    private final Delegate delegate;

    public FunctionalJsonLogic (Delegate delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        final JsonObject input = JsonRequest.wrap(request).readObject();
        final JsonObject output = delegate.handle(input);
        JsonResponse.wrap(response).send(output);
        return true;
    }
}
