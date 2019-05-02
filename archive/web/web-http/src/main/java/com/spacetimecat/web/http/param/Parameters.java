package com.spacetimecat.web.http.param;

/**
 * <p>
 *     Request parameters.
 * </p>
 */
public abstract class Parameters
{
    public abstract Param<String> get (String name);

    /**
     * @param name
     * parameter name
     * @throws MissingParamException if the request does not have the parameter
     * @see Param#required()
     */
    public final void require (String name)
    {
        get(name).required();
    }
}
