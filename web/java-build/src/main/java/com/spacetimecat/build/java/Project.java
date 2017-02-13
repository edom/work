package com.spacetimecat.build.java;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public final class Project
{
    private final Project parent;
    private Gav gav;

    private final List<Project> children = new ArrayList<>();

    private final List<Gav> dependencies = new ArrayList<>();

    public Project (Gav gav)
    {
        this(null, gav);
    }

    private Project (Project parent, Gav gav)
    {
        this.parent = parent;
        this.gav = gav;
    }

    public Project (String groupId, String artifactId, String version)
    {
        this(new Gav(groupId, artifactId, version));
    }

    public Project version (String s)
    {
        gav = new Gav(gav.getGroupId(), gav.getArtifactId(), s);
        return this;
    }

    public Project group (String s)
    {
        gav = new Gav(s, gav.getArtifactId(), gav.getVersion());
        return this;
    }

    public Project getChild (String artifactId)
    {
        return children.stream()
            .filter(p -> p.gav.getArtifactId().equals(artifactId))
            .findFirst()
            .orElseGet(() -> createChild(artifactId));
    }

    private Project createChild (String artifactId)
    {
        final Project p = new Project(gav.getGroupId(), artifactId, gav.getVersion());
        children.add(p);
        return p;
    }

    public Project child (String artifactId)
    {
        return child(artifactId, p -> {});
    }

    public Project child (String artifactId, Consumer<Project> configure)
    {
        final Project c = getChild(artifactId);
        configure.accept(c);
        return this;
    }

    public Project dependOn (Gav other)
    {
        dependencies.add(other);
        return this;
    }

    public Project dependOn (String gav)
    {
        return dependOn(Gav.parse(gav));
    }

    public Project dependOn (Project other)
    {
        return dependOn(other.gav);
    }

    public String name ()
    {
        return gav.getArtifactId();
    }

    public Pom pom ()
    {
        final Gav parent = this.parent == null ? null : this.parent.gav;
        final Pom p = new Pom(parent, gav);
        if (!children.isEmpty())
        {
            p.packaging("pom");
            final List<Project> children = new ArrayList<>(this.children);
            children.sort((a, b) -> a.name().compareTo(b.name()));
            children.forEach(c -> p.module(c.name()));
        }
        p.dependOn(dependencies);
        return p;
    }

    /**
     * <p>
     *     Generate pom.xml files recursively for this project and all its descendants.
     * </p>
     *
     * @param dir
     * the directory that will contain the pom.xml of this project
     */
    public void mavenize (String dir)
    {
        doGeneratePom(dir, 16);
    }

    public Project with (Consumer<Project> configure)
    {
        configure.accept(this);
        return this;
    }

    private void doGeneratePom (String targetDir, int depthLimit)
    {
        final File dir = new File(targetDir);
        if (!dir.mkdirs() && !dir.isDirectory())
        {
            throw new RuntimeException(
                "Could not create directory: " + dir.getAbsolutePath()
                    + ". Common causes: you don't have permission to write to some of its parent directories," +
                    "the filesystem runs out of free space or inode, or the path exists but doesn't point to a directory."
            );
        }
        pom().writeToFile(targetDir + "/pom.xml");
        children.forEach(c -> c.doGeneratePom(targetDir + "/" + c.name(), depthLimit - 1));
    }
}
