package com.spacetimecat.relational.jdbc2.example;

import javax.persistence.Table;

@Table(schema = "what", name = "supervise")
public interface SuperviseRow
{
    long getSupervisor ();

    long getSupervisee ();
}
