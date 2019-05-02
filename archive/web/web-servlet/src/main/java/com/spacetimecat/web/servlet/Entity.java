package com.spacetimecat.web.servlet;

import java.io.IOException;
import java.io.OutputStream;

public interface Entity
{
    String getMediaType ();

    void writeTo (OutputStream output) throws IOException;
}
