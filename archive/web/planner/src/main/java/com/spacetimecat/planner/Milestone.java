package com.spacetimecat.planner;

import java.util.Collection;
import java.util.Collections;

public final class Milestone
{
    private Collection<Milestone> prerequisites;
    private ManHour effort;
    private String description;

    public Milestone (String description, ManHour effort)
    {
        this.prerequisites = Collections.emptyList();
        this.effort = effort;
        this.description = description;
    }

    public String getDescription ()
    {
        return description;
    }

    public void setDescription (String description)
    {
        this.description = description;
    }

    public ManHour getEffort ()
    {
        return effort;
    }

    public void setEffort (ManHour effort)
    {
        this.effort = effort;
    }
}
