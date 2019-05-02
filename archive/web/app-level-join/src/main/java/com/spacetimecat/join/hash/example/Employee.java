package com.spacetimecat.join.hash.example;

public final class Employee
{
    private final int id;
    private final String name;
    private final Integer departmentId;

    public Employee (int id, String name, Integer departmentId)
    {
        this.id = id;
        this.name = name;
        this.departmentId = departmentId;
    }

    @Override
    public String toString ()
    {
        return name;
    }

    public Integer getDepartmentId ()
    {
        return departmentId;
    }
}
