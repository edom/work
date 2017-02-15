package com.spacetimecat.planner.web.dropwizard;

import com.spacetimecat.web.load.Load;
import com.spacetimecat.web.load.LoadFileBasic;
import com.spacetimecat.web.view.FileTemplate;
import com.spacetimecat.web.view.Template;

final class Development extends Deployment
{
    @Override
    Load loadStatic ()
    {
        return new LoadFileBasic("planner/src/main/resources/com/spacetimecat/planner/web/statics");
    }

    @Override
    Template template ()
    {
        return new FileTemplate("planner/src/main/resources/com/spacetimecat/planner/web/index.html");
    }

    @Override
    void showDevelopmentMode ()
    {
        warn(
            "\n" +
            "============================================================\n" +
            "                      Development Mode                      \n" +
            "============================================================"
        );
    }

    @Override
    void checkSanity ()
    {
    }
}
