package com.spacetimecat.build.java;

import com.spacetimecat.xml.dom.Document;
import com.spacetimecat.xml.dom.Element;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

final class Pom
{
    private static final List<Gav> defaultPlugins = Arrays.asList(
        "org.apache.maven.plugins:maven-compiler-plugin:3.6.1"
        , "org.apache.maven.plugins:maven-resources-plugin:3.0.2"
        , "org.apache.maven.plugins:maven-site-plugin:3.6"
    ).stream().map(Gav::parse).collect(Collectors.toList());

    private final Gav parent;
    private final Gav gav;

    private final List<Gav> dependencies = new ArrayList<>();
    private final List<Gav> plugins = new ArrayList<>();
    private final List<String> modules = new ArrayList<>();

    private String packaging = "jar";
    private String javaVersion = "1.8";

    Pom (Gav parent, Gav self)
    {
        this.parent = parent;
        this.gav = Objects.requireNonNull(self);
        this.plugins.addAll(defaultPlugins);
    }

    Pom (Gav gav)
    {
        this(null, gav);
    }

    Pom (String gav)
    {
        this(Gav.parse(gav));
    }

    Pom (String group, String artifact, String version)
    {
        this(new Gav(group, artifact, version));
    }

    Pom packaging (String p)
    {
        packaging = Objects.requireNonNull(p);
        return this;
    }

    Pom module (String m)
    {
        modules.add(Objects.requireNonNull(m));
        return this;
    }

    Pom dependOn (Gav gav)
    {
        dependencies.add(Objects.requireNonNull(gav));
        return this;
    }

    Pom dependOn (Gav... gavs)
    {
        return dependOn(Arrays.asList(gavs));
    }

    Pom dependOn (Collection<Gav> gavs)
    {
        dependencies.addAll(gavs);
        return this;
    }

    Pom dependOn (String colonSeparatedGav)
    {
        return dependOn(Gav.parse(colonSeparatedGav));
    }

    Pom dependOn (String group, String artifact, String version)
    {
        return dependOn(new Gav(group, artifact, version));
    }

    Document toXml ()
    {
        final String m = "http://maven.apache.org/POM/4.0.0";
        return Document.newEmpty()
            .elementNs(m, "project", p -> p
                .attributeNs(
                    "http://www.w3.org/2001/XMLSchema-instance"
                    , "xsi:schemaLocation"
                    , "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
                )
                .elementNs(m, "modelVersion", "4.0.0")
                .if_(() -> parent != null, pp -> pp
                    .elementNs(m, "parent", add(parent))
                )
                .with(add(gav))
                .elementNs(m, "packaging", packaging)
                .elementNs(m, "properties", q -> q
                    .elementNs(m, "maven.compiler.source", javaVersion)
                    .elementNs(m, "maven.compiler.target", javaVersion)
                )
                .if_(() -> packaging.equals("pom"), q -> q
                    .elementNs(m, "modules", r ->
                        modules.forEach(c -> r.elementNs(m, "module", c))
                    )
                )
                .if_(() -> !dependencies.isEmpty(), u -> u
                    .elementNs(m, "dependencies", d ->
                    {
                        dependencies.forEach(e -> d.elementNs(m, "dependency", add(e)));
                    })
                )
                .elementNs(m, "build", b -> b
                    .elementNs(m, "pluginManagement", pm -> pm
                        .elementNs(m, "plugins", pluginsElement ->
                        {
                            plugins.forEach(plugin -> pluginsElement.elementNs(m, "plugin", add(plugin)));
                        }
                        )
                    )
                )
            );
    }

    private static Consumer<Element> add (Gav gav)
    {
        final String m = "http://maven.apache.org/POM/4.0.0";
        return n -> n
            .elementNs(m, "groupId", gav.getGroupId())
            .elementNs(m, "artifactId", gav.getArtifactId())
            .elementNs(m, "version", gav.getVersion())
            ;
    }

    @Override
    public String toString ()
    {
        return toXml().toString();
    }

    public void writeToFile (String path)
    {
        toXml().writeToFile(path);
    }
}
