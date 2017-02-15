package com.spacetimecat.planner.web.dropwizard;

import com.spacetimecat.web.load.Load;
import com.spacetimecat.web.load.LoadResource;
import com.spacetimecat.web.load.NoSuchFileException;
import com.spacetimecat.web.view.ResourceTemplate;
import com.spacetimecat.web.view.Template;

final class Production extends Deployment
{
    private final ClassLoader staticsClassLoader;

    Production (ClassLoader staticsClassLoader)
    {
        this.staticsClassLoader = staticsClassLoader;
    }

    @Override
    Load loadStatic ()
    {
        return new LoadResource(staticsClassLoader, "com/spacetimecat/planner/web/statics/");
    }

    @Override
    Template template ()
    {
        return new ResourceTemplate(
            staticsClassLoader
            , "com/spacetimecat/planner/web/index.html"
            , "http://localhost:8080/"
        );
    }

    @Override
    void checkSanity ()
    {
        super.checkSanity();

        final Load load = loadStatic();

        try
        {
            load.load("style.css");
        }
        catch (NoSuchFileException e)
        {
            throw new SanityCheckError("The static resource loader could not load 'style.css'", e);
        }

        try
        {
            load.load("../statics/style.css");
            throw new SanityCheckError("The static resource loader is vulnerable to path traversal (../statics/style.css)");
        }
        catch (NoSuchFileException e)
        {
            // Expected.
        }

        try
        {
            load.load("../index.html");
            throw new SanityCheckError("The static resource loader is vulnerable to path traversal (../index.html)");
        }
        catch (NoSuchFileException e)
        {
            // Expected.
        }
    }
}
