package com.spacetimecat.build.java;

import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Writer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.nio.file.Paths;

public final class GenerateMavenPomXmlFiles
{
    private final Path dir;
    private final Project project;

    /**
     * @param dir
     * target directory that will contain the generated pom.xml file
     *
     * @param project
     * project description
     */
    public GenerateMavenPomXmlFiles (String dir, Project project)
    {
        this(Paths.get(dir), project);
    }

    private GenerateMavenPomXmlFiles (Path dir, Project project)
    {
        this.dir = dir;
        this.project = project;
    }

    public void run ()
    {
        recur(16);
    }

    private void recur (int depthLimit)
    {
        if (depthLimit <= 0)
        {
            throw new ProjectTooDeepException(dir.resolve(project.path()).toString());
        }
        final File dir = this.dir.toFile();
        if (!dir.mkdirs() && !dir.isDirectory())
        {
            throw new RuntimeException(
                "Could not create directory: " + dir.getAbsolutePath()
                    + ". Common causes: you don't have permission to write to some of its parent directories," +
                    "the filesystem runs out of free space or inode, or the path exists but doesn't point to a directory."
            );
        }
        writeTo(project.mavenModel(), this.dir.resolve("pom.xml"));
        project.children().forEach(c ->
            new GenerateMavenPomXmlFiles(this.dir.resolve(c.path()), c)
                .recur(depthLimit - 1)
        );
    }

    private static void writeTo (Model self, Path path)
    {
        try (FileOutputStream s = new FileOutputStream(path.toFile()))
        {
            new MavenXpp3Writer().write(s, self);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
