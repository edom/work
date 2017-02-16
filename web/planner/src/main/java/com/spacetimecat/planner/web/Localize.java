package com.spacetimecat.planner.web;

import com.spacetimecat.web.view.Document2;

final class Localize
{
    private final Document2 d;

    Localize (Document2 d)
    {
        this.d = d;
    }

    void number (String cssClassName, int count)
    {
        d.select("." + cssClassName).forEach(e ->
        {
            if (count == 0 && e.hasClass("zero")) { return; }
            if (count == 1 && e.hasClass("one")) { return; }
            if (count > 1 && e.hasClass("many")) { return; }
            e.remove();
        });
    }
}
