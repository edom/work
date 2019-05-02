package com.spacetimecat.build.java;

import java.util.Objects;

final class Gav implements HasGav
{
    private final String groupId;
    private final String artifactId;
    private final String version;

    /**
     * @param groupId
     * can be null
     *
     * @param artifactId
     * cannot be null
     *
     * @param version
     * can be null
     */
    Gav (String groupId, String artifactId, String version)
    {
        this.groupId = groupId;
        this.artifactId = Objects.requireNonNull(artifactId);
        this.version = version;
    }

    static Gav parse (String s)
    {
        final String[] c = s.split(":");
        if (c.length != 3) { throw new IllegalArgumentException(); }
        return new Gav(c[0], c[1], c[2]);
    }

    @Override
    public String getGroupId ()
    {
        return groupId;
    }

    @Override
    public String getArtifactId ()
    {
        return artifactId;
    }

    @Override
    public String getVersion ()
    {
        return version;
    }
}
