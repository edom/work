package com.spacetimecat.build.java;

import java.util.Objects;

final class Gav
{
    private final String group;
    private final String artifact;
    private final String version;

    /**
     * @param group
     * can be null
     *
     * @param artifact
     * cannot be null
     *
     * @param version
     * can be null
     */
    Gav (String group, String artifact, String version)
    {
        this.group = group;
        this.artifact = Objects.requireNonNull(artifact);
        this.version = version;
    }

    static Gav parse (String s)
    {
        final String[] c = s.split(":");
        if (c.length != 3) { throw new IllegalArgumentException(); }
        return new Gav(c[0], c[1], c[2]);
    }

    public String getGroup ()
    {
        return group;
    }

    public String getArtifact ()
    {
        return artifact;
    }

    public String getVersion ()
    {
        return version;
    }
}
