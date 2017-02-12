package com.spacetimecat.build.java;

import com.spacetimecat.build.files.FilesUnder;
import com.spacetimecat.build.files.Paths;
import com.spacetimecat.xml.dom.select.Child;
import com.spacetimecat.xml.dom.select.Descendant;
import com.spacetimecat.xml.dom.select.Named;
import com.spacetimecat.xml.dom.select.Select;

public final class BuildExampleMain
{
    public static void main (String[] args) throws Exception
    {
        final String root = "/home/erik/web/build-test";
        System.out.println(new FilesUnder("").endingWith(".java").get());
        final Paths projects = Paths.under(root).whichLooksLikeMavenProject();
        final String jettyVersion = "[9,10)";
        final Pom pom = new Pom("com.spacetimecat", "everything", "0.0.0-SNAPSHOT")
            .packaging("pom")
            .module("foo")
            .dependOn("org.eclipse.jetty:jetty-server:" + jettyVersion)
            .dependOn("org.eclipse.jetty:jetty-servlet:" + jettyVersion);
        System.out.println(projects);
        System.out.println(Paths.under("").whoseNameEndsWith(".java"));
        System.out.println(pom);
        System.out.println(
            new Select(pom.toXml())
                .then(new Child())
                .then(new Descendant())
                .then(new Named("module"))
                .collect());
//        pom.writeToFile(root + "/pom.xml");

//        new DefaultJavaCompiler().compile(new File("compile-output"), new FilesUnder("src"));
    }
}
