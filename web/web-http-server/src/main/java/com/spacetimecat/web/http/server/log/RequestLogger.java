package com.spacetimecat.web.http.server.log;

@FunctionalInterface
public interface RequestLogger
{
    void log (LogEntry entry);
}
