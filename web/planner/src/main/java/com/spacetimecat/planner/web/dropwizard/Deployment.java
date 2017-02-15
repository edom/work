package com.spacetimecat.planner.web.dropwizard;

import com.spacetimecat.planner.web.WebsiteResource;
import com.spacetimecat.web.load.Load;
import com.spacetimecat.web.view.Template;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Properties;

abstract class Deployment
{
    private static final Logger logger = LoggerFactory.getLogger(Deployment.class);

    abstract Load loadStatic ();
    abstract Template template ();

    final WebsiteResource website ()
    {
        return new WebsiteResource(loadStatic(), template());
    }

    final void warn (String message)
    {
        logger.warn(message);
    }

    void showDevelopmentMode () {}

    /**
     * <p>
     *     The default does nothing.
     * </p>
     *
     * @throws SanityCheckError
     * if something is very wrong
     */
    void checkSanity () {}

    static Deployment create ()
    {
        final Thread thread = Thread.currentThread();
        final ClassLoader scl = thread.getContextClassLoader();
        logger.info("The context class loader of {} is {}", thread, scl);
        final Deployment deployment = isDevelopment() ? new Development() : new Production(scl);
        deployment.showDevelopmentMode();
        deployment.checkSanity();
        return deployment;
    }

    private static boolean isDevelopment ()
    {
        final Map<String, String> environments = System.getenv();
        final Properties properties = System.getProperties();
        return
            environments.containsKey("DEVELOP")
            || properties.containsKey("develop")
            ;
    }
}
