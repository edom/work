package com.spacetimecat.planner.web.dropwizard;

import com.spacetimecat.planner.web.WebsiteResource;
import io.dropwizard.setup.Environment;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class Application extends io.dropwizard.Application<Configuration>
{
    private static final Logger logger = LoggerFactory.getLogger(Application.class);

    @Override
    public void run (Configuration config, Environment environment) throws Exception
    {
        final Deployment deployment = Deployment.create();
        final WebsiteResource website = deployment.website();
        environment.jersey().register(website);
    }

    public static void main (String[] args) throws Exception
    {
        new Application().run("server");
    }
}
