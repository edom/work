package com.spacetimecat.web.http.param;

import com.spacetimecat.web.http.BadRequestException;

public final class MissingParamException extends BadRequestException
{
    public MissingParamException (String message)
    {
        super(message);
    }
}
