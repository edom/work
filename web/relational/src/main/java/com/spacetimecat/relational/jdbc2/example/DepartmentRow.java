package com.spacetimecat.relational.jdbc2.example;

import javax.persistence.Table;

@Table(schema = "what", name = "department")
public interface DepartmentRow
{
    long getId ();

    String getName ();
}
