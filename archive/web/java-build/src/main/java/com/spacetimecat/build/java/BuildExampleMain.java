package com.spacetimecat.build.java;

import com.spacetimecat.build.files.FilesUnder;
import com.spacetimecat.build.files.Paths;

public final class BuildExampleMain
{
    public static void main (String[] args) throws Exception
    {
        final String root = "/home/erik/web/build-test";
        System.out.println(new FilesUnder("").endingWith(".java").get());
        final Paths projects = Paths.under(root).whichLooksLikeMavenProject();
        System.out.println(projects);
        System.out.println(Paths.under("").whoseNameEndsWith(".java"));
    }
}
