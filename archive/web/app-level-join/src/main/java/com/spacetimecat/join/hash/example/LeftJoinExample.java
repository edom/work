package com.spacetimecat.join.hash.example;

import com.spacetimecat.join.hash.HashLeftEquiJoin2;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public final class LeftJoinExample
{
    public static void main (String[] args)
    {
        final List<Employee> employees = Arrays.asList(
            new Employee(0, "Alice", 10)
            , new Employee(1, "Bob", 11)
            , new Employee(2, "Charlie", 12)
            , new Employee(3, "Dan", null)
            , new Employee(4, "Eve", 14)
        );

        final List<Department> departments = Arrays.asList(
            new Department(10, "Annihilation")
            , new Department(11, "Brawling")
            , new Department(12, "Conquest")
            , new Department(13, "Doodling")
            , new Department(14, "Eavesdropping")
        );

        final Collection<WorkingEmployee> assignments =
            new HashLeftEquiJoin2<>(Employee::getDepartmentId, Department::getId, WorkingEmployee::new)
                .apply(employees, departments);

        assignments.forEach(System.out::println);
    }
}
