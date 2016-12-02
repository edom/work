package com.spacetimecat.build.plugin.gradle;

import com.spacetimecat.build.dependency.ConstraintAccumulator;
import com.spacetimecat.build.dependency.Version;
import com.spacetimecat.build.math.extended.Extended;
import com.spacetimecat.build.math.range.Range;
import com.spacetimecat.build.version.string.GradleVersionRange;
import com.spacetimecat.build.version.string.GradleVersionRangeString;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.artifacts.ModuleVersionSelector;
import org.gradle.api.artifacts.ResolutionStrategy;
import org.gradle.api.logging.Logger;

public final class DependencyResolutionPlugin implements Plugin<Project>
{
    private static final String className = DependencyResolutionPlugin.class.getName();

    private Logger logger;

    @Override
    public void apply (Project project)
    {
        logger = project.getLogger();
        project.getConfigurations().forEach(configuration ->
        {
            final ResolutionStrategy strategy = configuration.getResolutionStrategy();
            final ConstraintAccumulator accumulator = new ConstraintAccumulator();
            strategy.eachDependency(details ->
            {
                final ModuleVersionSelector target = details.getTarget();
                final String group = target.getGroup();
                final String name = target.getName();
                final String longName = String.format("%s:%s", group, name);
                final String oldConstraint = target.getVersion();
                final GradleVersionRangeString internal = new GradleVersionRangeString(oldConstraint);
                final Range<Extended<Version>> constraint = internal.parse();
                final Range<Extended<Version>> intersection = accumulator.add(longName, constraint);
                final String newConstraint = new GradleVersionRange(intersection).toString();
                info(String.format("%-40s %20s -> %-20s", longName, oldConstraint, newConstraint));
                details.useVersion(newConstraint);
            });
        });
    }

    private void info (String message)
    {
        logger.info(String.format("%s: %s", className, message));
    }

}
