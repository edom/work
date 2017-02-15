package com.spacetimecat.planner.web.dropwizard;

import com.spacetimecat.planner.web.WebsiteResource;
import com.spacetimecat.web.load.Load;
import com.spacetimecat.web.load.LoadFileBasic;
import com.spacetimecat.web.view.FileTemplate;
import com.spacetimecat.web.view.Template;
import io.dropwizard.setup.Environment;

public final class Application extends io.dropwizard.Application<Configuration>
{
    @Override
    public void run (Configuration config, Environment environment) throws Exception
    {
        final Load loadStatic = new LoadFileBasic("planner/src/main/resources/com/spacetimecat/planner/web/statics");
        final Template template = new FileTemplate("planner/src/main/resources/com/spacetimecat/planner/web/index.html");
        environment.jersey().register(new WebsiteResource(loadStatic, template));
    }

    public static void main (String[] args) throws Exception
    {
        new Application().run("server");
    }
}
