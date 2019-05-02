package com.spacetimecat.maven.plugin.deploy;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;

import java.io.*;
import java.util.Arrays;

@Mojo
(
    name = "deploy"
    , defaultPhase = LifecyclePhase.VALIDATE
    , requiresDependencyResolution = ResolutionScope.RUNTIME_PLUS_SYSTEM
    , requiresProject = false
)
public final class DeployMojo extends AbstractMojo
{
    @Parameter
    (
        name = "groupId"
        , property = "groupId"
        , required = true
    )
    private String groupId;

    @Parameter
    (
        name = "artifactId"
        , property = "artifactId"
        , required = true
    )
    private String artifactId;

    @Parameter
    (
        name = "version"
        , property = "version"
        , required = true
    )
    private String version;

    @Parameter
    (
        name = "mainClass"
        , property = "mainClass"
        , required = true
    )
    private String mainClass;

    @Override
    public void execute () throws MojoExecutionException, MojoFailureException
    {
        try
        {
            doExecute();
        }
        catch (MojoExecutionException | MojoFailureException | RuntimeException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new MojoExecutionException("Unhandled exception", e);
        }
    }

    private void doExecute () throws Exception
    {
        Arrays.asList("download", "run", "runStale").forEach(x ->
        {
            final File f = unpack(x);
            final boolean ownerOnly = false;
            if (!f.setExecutable(true, ownerOnly))
            {
                throw new RuntimeException("Could not make file executable: " + x);
            }
        });

        DeployMojo.unpack("pom.xml");

        set("Goal", "com.spacetimecat.maven.plugin:deploy-maven-plugin:0.0.0-SNAPSHOT:deploy");

        set("GroupId", groupId);
        set("ArtifactId", artifactId);
        set("Version", version);
        set("MainClass", mainClass);

        ensure("JavaArguments");
    }

    private static boolean ensure (String path)
    {
        try
        {
            return new File(path).createNewFile();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    private static void set (String path, String value)
    {
        try (PrintStream out = new PrintStream(new FileOutputStream(path)))
        {
            out.println(value);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    private static File unpack (String path)
    {
        final File file = new File(path);
        try (
            InputStream in = DeployMojo.class.getResourceAsStream(path);
            OutputStream out = new FileOutputStream(path)
        )
        {
            final byte[] buf = new byte[4096];
            for (;;)
            {
                int count = in.read(buf);
                if (count < 0) { break; }
                out.write(buf, 0, count);
            }
            return file;
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
