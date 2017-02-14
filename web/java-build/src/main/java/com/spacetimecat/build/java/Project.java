package com.spacetimecat.build.java;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Consumer;

public final class Project
{
    private final List<Gav> dependencies = new ArrayList<>();
    private final List<Project> children = new ArrayList<>();
    private final String path;
    private final Project parent;
    private String groupId = "groupId";
    private String artifactId = "artifactId";
    private String version = "0.0.0-SNAPSHOT";

    /**
     * @param path
     * path relative to the path of the parent project
     * (not necessarily the root project)
     */
    public Project (String path)
    {
        this(null, path);
    }

    private Project (Project parent, String path)
    {
        this.parent = parent;
        this.path = path;
    }

    public Project with (Consumer<Project> conf)
    {
        conf.accept(this);
        return this;
    }

    private Gav gav () { return new Gav(groupId, artifactId, version); }

    public String path () { return path; }
    private boolean pathIs (String s) { return Paths.get(path).equals(Paths.get(s)); }

    public String group () { return groupId; }
    public Project group (String s) { groupId = s; return this; }

    public String artifact () { return artifactId; }
    public Project artifact (String s) { artifactId = s; return this; }

    public String version () { return version; }
    public Project version (String s) { version = s; return this; }

    public Project dependOn (Gav gav) { dependencies.add(gav); return this; }
    public Project dependOn (String gav) { return dependOn(Gav.parse(gav)); }
    public Project dependOn (Project p) { return dependOn(p.gav()); }

    public Project getChild (String path)
    {
        return ensureChild(path);
    }

    public Project child (String path)
    {
        return child(path, c -> {});
    }

    public Project child (String path, Consumer<Project> configure)
    {
        final Project child = ensureChild(path);
        configure.accept(child);
        return this;
    }

    private Project ensureChild (String path)
    {
        return children.stream()
            .filter(p -> p.pathIs(path))
            .findAny()
            .orElseGet(() -> createChild(path));
    }

    private Project createChild (String path)
    {
        final Path name = Paths.get(path).getFileName();
        if (name == null) { throw new IllegalArgumentException(path); }
        final Project child = new Project(path)
            .group(groupId)
            .artifact(name.toString())
            .version(version);
        children.add(child);
        return child;
    }

    List<Project> children () { return children; }

    Pom pom ()
    {
        final Gav parent = this.parent == null ? null : this.parent.gav();
        final Pom p = new Pom(parent, gav());
        if (!children.isEmpty())
        {
            p.packaging("pom");
            final List<Project> children = new ArrayList<>(this.children);
            children.sort(Comparator.comparing(Project::path));
            children.forEach(c -> p.module(c.path()));
        }
        p.dependOn(dependencies);
        return p;
    }
}
