package com.spacetimecat.relational.jdbc2.example;

public final class Employee implements EmployeeRow
{
    private final EmployeeRow employee;
    private final EmployeeRow boss;

    public Employee (EmployeeRow employee, EmployeeRow boss)
    {
        this.employee = employee;
        this.boss = boss;
    }

    @Override
    public long getId ()
    {
        return employee.getId();
    }

    @Override
    public long getDepartmentId ()
    {
        return employee.getDepartmentId();
    }

    @Override
    public String getName ()
    {
        return employee.getName();
    }

    @Override
    public int getSalary ()
    {
        return employee.getSalary();
    }

    public boolean isTopBoss ()
    {
        return boss == null;
    }

    public boolean isRich ()
    {
        return getSalary() >= 50000;
    }
}
