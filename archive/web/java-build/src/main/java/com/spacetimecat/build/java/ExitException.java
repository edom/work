package com.spacetimecat.build.java;

import java.util.List;

public final class ExitException extends RuntimeException
{
    public ExitException (int code, List<String> command)
    {
        super("Subprocess exited with code " + code + " while executing command: " + command);
    }
}
