package com.spacetimecat.build.java;

import org.apache.maven.model.*;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

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
        final Project child = new Project(this, path)
            .group(groupId)
            .artifact(name.toString())
            .version(version)
            ;
        children.add(child);
        return child;
    }

    List<Project> children () { return children; }

    Model mavenModel ()
    {
        final Model m = new Model();

        m.setModelVersion("4.0.0");
        final Properties properties = new Properties();
        final String javaVersion = "1.8";
        properties.put("maven.compiler.source", javaVersion);
        properties.put("maven.compiler.target", javaVersion);
        m.setProperties(properties);

        final List<Plugin> defaultPlugins = Arrays.asList(
            "org.apache.maven.plugins:maven-compiler-plugin:3.6.1"
            , "org.apache.maven.plugins:maven-resources-plugin:3.0.2"
            , "org.apache.maven.plugins:maven-site-plugin:3.6"
        ).stream().map(Gav::parse).map(Project::plugin).collect(Collectors.toList());

        final Build build = new Build();
        PluginManagement pluginManagement = new PluginManagement();
        pluginManagement.setPlugins(defaultPlugins);
        build.setPluginManagement(pluginManagement);
        m.setBuild(build);

        if (parent != null)
        {
            final Parent p = new Parent();
            p.setGroupId(parent.group());
            p.setArtifactId(parent.artifact());
            p.setVersion(parent.version());
            m.setParent(p);
        }
        if (!children.isEmpty())
        {
            m.setPackaging("pom");
            final List<Project> children = new ArrayList<>(this.children);
            children.sort(Comparator.comparing(Project::path));
            children.forEach(c -> m.addModule(c.path()));
        }
        m.setGroupId(groupId);
        m.setArtifactId(artifactId);
        m.setVersion(version);
        for (Gav gav : dependencies)
        {
            m.addDependency(dependency(gav));
        }
        return m;
    }

    private static Plugin plugin (Gav gav)
    {
        final Plugin p = new Plugin();
        p.setGroupId(gav.getGroupId());
        p.setArtifactId(gav.getArtifactId());
        p.setVersion(gav.getVersion());
        return p;
    }

    private static Dependency dependency (Gav gav)
    {
        final Dependency d = new Dependency();
        d.setGroupId(gav.getGroupId());
        d.setArtifactId(gav.getArtifactId());
        d.setVersion(gav.getVersion());
        return d;
    }

    private static Dependency dependency (String strGav)
    {
        return dependency(Gav.parse(strGav));
    }
}
