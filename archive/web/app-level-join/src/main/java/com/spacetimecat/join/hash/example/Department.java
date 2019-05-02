package com.spacetimecat.join.hash.example;

public final class Department
{
    private final int id;
    private final String name;

    public Department (int id, String name)
    {
        this.id = id;
        this.name = name;
    }

    @Override
    public String toString ()
    {
        return name;
    }

    public int getId ()
    {
        return id;
    }
}
