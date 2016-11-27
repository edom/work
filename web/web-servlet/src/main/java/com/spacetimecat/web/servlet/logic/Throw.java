package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Objects;
import java.util.function.Supplier;

public final class Throw implements Logic
{
    private final Supplier<Throwable> throwable;

    public Throw (Supplier<Throwable> throwable)
    {
        this.throwable = Objects.requireNonNull(throwable, "throwable");
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        final Throwable throwable = Objects.requireNonNull(this.throwable.get(), "throwable");
        if (throwable instanceof RuntimeException) { throw (RuntimeException) throwable; }
        if (throwable instanceof ServletException) { throw (ServletException) throwable; }
        if (throwable instanceof IOException) { throw (IOException) throwable; }
        if (throwable instanceof Error) { throw (Error) throwable; }
        throw new AssertionError(throwable);
    }
}
