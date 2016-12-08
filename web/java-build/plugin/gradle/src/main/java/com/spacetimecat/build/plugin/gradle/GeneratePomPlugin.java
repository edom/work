package com.spacetimecat.build.plugin.gradle;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.plugins.Convention;
import org.gradle.api.plugins.JavaPluginConvention;
import org.gradle.api.plugins.MavenPluginConvention;
import org.gradle.api.plugins.PluginContainer;

public final class GeneratePomPlugin implements Plugin<Project>
{
    @Override
    public void apply (Project project)
    {
        final PluginContainer plugins = project.getPlugins();
        plugins.apply("java");
        plugins.apply("maven");
        final Convention convention = project.getConvention();
        final JavaPluginConvention java = convention.getPlugin(JavaPluginConvention.class);
        final MavenPluginConvention maven = convention.getPlugin(MavenPluginConvention.class);
        project.task("generatePom").doLast(task ->
        {
            // Workaround for Gradle non-implementation.
            maven.pom().withXml(xml ->
            {
                final Element pom = new Element(xml.asElement());

                final Element build = pom.appendChild("build");
                {
                    final Element ePlugins = build.appendChild("plugins");

                    // https://maven.apache.org/plugin-developers/cookbook/attach-source-javadoc-artifacts.html

                    final Element ePlugin = ePlugins.appendChild("plugin");
                    ePlugin.appendChild("groupId").setText("org.apache.maven.plugins");
                    ePlugin.appendChild("artifactId").setText("maven-source-plugin");
                    final Element execution = ePlugin.appendChild("executions").appendChild("execution");
                    execution.appendChild("id").setText("attach-sources");
                    execution.appendChild("goals").appendChild("goal").setText("jar");
                }

                final Element properties = pom.appendChild("properties");

                final Element source = properties.appendChild("maven.compiler.source");
                source.setText(java.getSourceCompatibility().toString());

                final Element target = properties.appendChild("maven.compiler.target");
                target.setText(java.getTargetCompatibility().toString());
            }).writeTo("pom.xml");
        });
    }
}
