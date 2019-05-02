package com.spacetimecat.join.hash.example;

import java.util.Objects;

public final class WorkingEmployee
{
    private final Employee employee;
    private final Department department;

    public WorkingEmployee (Employee employee, Department department)
    {
        this.employee = Objects.requireNonNull(employee);
        this.department = department;
    }

    @Override
    public String toString ()
    {
        return
            department == null
                ? String.format("%s is not working", employee)
                : String.format("%s works in %s", employee, department);
    }
}
