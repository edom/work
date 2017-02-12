package com.spacetimecat.planner;

import java.util.Collection;

public final class Milestone
{
    private Collection<Milestone> prerequisites;
    private ManHour effort;
    private String what;
}
