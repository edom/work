package com.spacetimecat.relational.jdbc2.example;

import javax.persistence.Id;
import javax.persistence.Table;

@Table(schema = "what", name = "employee")
public interface EmployeeRow
{
    @Id
    long getId ();

    long getDepartmentId ();

    @Order(100)
    String getName ();

    @Order(200)
    int getSalary ();
}
