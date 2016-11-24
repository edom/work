package com.spacetimecat.web.servlet;

public final class MissingParamException extends BadRequestException
{
    public MissingParamException (String message)
    {
        super(message);
    }
}
