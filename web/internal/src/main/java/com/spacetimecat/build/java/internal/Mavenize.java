package com.spacetimecat.build.java.internal;

import com.spacetimecat.build.java.GenerateMavenPomXmlFiles;
import com.spacetimecat.build.java.Project;

import java.io.File;

final class Mavenize
{
    public static void main (String[] args)
    {
        final String root = new File("").getAbsolutePath();
        final String jettyVersion = "[9,10)";
        final String logback = "ch.qos.logback:logback-classic:[1.1.7,2)";
        final String slf4j = "org.slf4j:slf4j-api:[1.7.21,2)";
        final Project project = new Project("")
        .group("com.spacetimecat")
        .artifact("everything")
        .version("0.0.0")
        .plugin("org.apache.maven.plugins:maven-compiler-plugin:3.6.1")
        .plugin("org.apache.maven.plugins:maven-resources-plugin:3.0.2")
        .plugin("org.apache.maven.plugins:maven-site-plugin:3.6")
        .with(r -> r
            .child("app-level-join", c -> c
                .version("0.0.0-SNAPSHOT")
                .dependOn(r.getChild("java-lang-function"))
            )
            .child("concurrent", concurrent -> concurrent
                .version("0.0.0")
                .child("batch", c -> c
                    .version("0.0.0-SNAPSHOT")
                    .dependOn(r.getChild("java-lang"))
                )
                .child("lock", c -> c
                    .version("0.0.0")
                    .dependOn(r.getChild("java-lang"))
                )
                .child("lock-client", c -> c
                    .version("0.0.0")
                    .dependOn(concurrent.getChild("lock-network"))
                    .dependOn(logback)
                    .dependOn(slf4j)
                )
                .child("lock-example", c -> c
                    .version("0.0.0-SNAPSHOT")
                    .dependOn(concurrent.getChild("lock-client"))
                    .dependOn(concurrent.getChild("lock-server"))
                )
                .child("lock-network", c -> c
                    .version("0.0.0")
                    .dependOn(concurrent.getChild("lock"))
                    .dependOn(r.getChild("server"))
                )
                .child("lock-server", c -> c
                    .version("0.0.0")
                    .dependOn(concurrent.getChild("lock-network"))
                    .dependOn(r.getChild("server"))
                    .dependOn(logback)
                    .dependOn(slf4j)
                )
            )
            .child("internal", i -> i
                .version("0.0.0-SNAPSHOT")
                .group("com.spacetimecat.internal")
                .dependOn(r.getChild("java-build"))
            )
            .child("io", c ->
                c.version("0.0.0")
            )
            .child("java-build", c -> c
                .version("0.0.0-SNAPSHOT")
                .dependOn(r.getChild("java-lang"))
                .dependOn("org.apache.maven:maven-model:[3,)")
            )
            .child("java-lang", c -> c
                .version("0.0.0")
            )
            .child("java-lang-function", c -> c
                .version("0.0.0")
            )
            .child("java-util-concurrent", c -> c
                .version("0.0.0")
            )
            .child("maven-plugin", mp -> mp
                .group("com.spacetimecat.maven.plugin")
                .version("0.0.0")
                .plugin("org.apache.maven.plugins:maven-plugin-plugin:3.5")
                .child("deploy-maven-plugin", c -> c
                    .packaging("maven-plugin")
                    .version("0.0.0")
                    .dependOn("org.apache.maven:maven-plugin-api:3.3.9")
                    .dependOn("org.apache.maven.plugin-tools:maven-plugin-annotations:3.5")
                )
            )
            .child("planner", c -> c
                .dependOn("io.dropwizard:dropwizard-core:1.0.6")
                .dependOn(r.getChild("web-view"))
            )
            .child("relational", c -> c
                .dependOn("com.h2database:h2:1.4.193")
                .dependOn("org.hibernate.javax.persistence:hibernate-jpa-2.1-api:1.0.0.Final")
            )
            .child("server", c -> c
                .version("0.0.0")
                .dependOn(r.getChild("io"))
            )
            .child("web-all", c -> c
                .dependOn(r.getChild("io"))
                .dependOn(r.getChild("web-http"))
                .dependOn(r.getChild("web-http-server"))
                .dependOn(r.getChild("web-server"))
                .dependOn(r.getChild("web-servlet"))
                .dependOn(r.getChild("web-servlet-json"))
                .dependOn(r.getChild("web-view"))
            )
            .child("web-example", c -> c
                .dependOn(r.getChild("web-all"))
            )
            .child("web-example-http-server", c -> c
                .dependOn(r.getChild("web-http-server"))
            )
            .child("web-example-json-api", c -> c
                .dependOn(r.getChild("web-all"))
            )
            .child("web-http", c -> c
                .dependOn(r.getChild("io"))
            )
            .child("web-http-server", c -> c
                .dependOn(r.getChild("server"))
                .dependOn(r.getChild("web-http"))
            )
            .child("web-server", c -> c
                .dependOn(r.getChild("server"))
                .dependOn(r.getChild("web-http"))
                .dependOn("org.eclipse.jetty:jetty-server:" + jettyVersion)
                .dependOn("org.eclipse.jetty:jetty-servlet:" + jettyVersion)
            )
            .child("web-servlet", c -> c
                .dependOn(r.getChild("web-http"))
                .dependOn("javax.servlet:javax.servlet-api:[3.1,4)")
            )
            .child("web-servlet-json", c -> c
                .dependOn(r.getChild("web-servlet"))
                .dependOn("org.glassfish:javax.json:[1,2)")
            )
            .child("web-view", c -> c
                .dependOn("org.jsoup:jsoup:[1.7,2)")
            )
            .child("xml-dom")
            .child("xml-dom-select", c -> c
                .dependOn(r.getChild("xml-dom"))
            )
        );
        new GenerateMavenPomXmlFiles(root, project).run();
    }
}
