package com.spacetimecat.build.version.string;

import com.spacetimecat.build.dependency.Version;
import com.spacetimecat.build.math.vector.Vector;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * <p>
 *     {@link String} representing a version such as {@code 1.2.3}.
 * </p>
 */
public final class VersionString
{
    private final String string;

    public VersionString (String string)
    {
        this.string = string;
    }

    public Version parse ()
    {
        final List<String> parts = new ArrayList<>();
        final StringTokenizer tokenizer = new StringTokenizer(string,".-", true);
        while (tokenizer.hasMoreTokens())
        {
            parts.add(tokenizer.nextToken());
        }
        return new Version(Vector.create(parts));
    }

    @Override
    public String toString ()
    {
        return string;
    }
}
