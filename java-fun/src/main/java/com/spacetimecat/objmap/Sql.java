package com.spacetimecat.objmap;

import com.spacetimecat.collection.FiniteIterable;
import com.spacetimecat.collection.Iterables;

final class Sql
{
    private Sql () {}
    static String makeInsertValuesStatement (String tableName, FiniteIterable<String> columnNames)
    {
        final int n = columnNames.size();
        return
            "INSERT INTO " + tableName + " ("
            + columnNames.intersperse(",").fold("", (String a, String b) -> a + b)
            + ") VALUES ("
            + Iterables.replicate(n, "?").intersperse(",")
            + ")"
            ;
    }
}
