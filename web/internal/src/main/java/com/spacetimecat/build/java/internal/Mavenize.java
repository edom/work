package com.spacetimecat.build.java.internal;

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
        new Project("com.spacetimecat", "everything", "0.0.0-SNAPSHOT").with(r -> r
            .child("app-level-join", c -> c
                .dependOn(r.getChild("java-lang-function"))
            )
            .child("concurrent", concurrent -> concurrent
                .child("batch", c -> c.dependOn(r.getChild("java-lang")))
                .child("lock", c -> c.dependOn(r.getChild("java-lang")))
                .child("lock-client", c -> c
                    .dependOn(concurrent.getChild("lock-network"))
                    .dependOn(logback)
                    .dependOn(slf4j)
                )
                .child("lock-example", c -> c
                    .dependOn(concurrent.getChild("lock-client"))
                    .dependOn(concurrent.getChild("lock-server"))
                )
                .child("lock-network", c -> c
                    .dependOn(concurrent.getChild("lock"))
                    .dependOn(r.getChild("server"))
                )
                .child("lock-server", c -> c
                    .dependOn(concurrent.getChild("lock-network"))
                    .dependOn(r.getChild("server"))
                    .dependOn(logback)
                    .dependOn(slf4j)
                )
            )
            .child("internal", i -> i
                .group("com.spacetimecat.internal")
                .dependOn(r.getChild("java-build"))
            )
            .child("io")
            .child("java-build", c -> c
                .dependOn(r.getChild("java-lang"))
                .dependOn(r.getChild("xml-dom"))
                .dependOn(r.getChild("xml-dom-select"))
            )
            .child("java-lang")
            .child("java-lang-function")
            .child("java-util-concurrent")
            .child("planner")
            .child("relational", c -> c
                .dependOn("com.h2database:h2:1.4.193")
                .dependOn("org.hibernate.javax.persistence:hibernate-jpa-2.1-api:1.0.0.Final")
            )
            .child("server", c -> c
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
        ).mavenize(root);
    }
}
