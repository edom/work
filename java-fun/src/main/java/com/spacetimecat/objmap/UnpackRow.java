package com.spacetimecat.objmap;

import com.spacetimecat.collection.Iterator;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

public interface UnpackRow<A> extends BasicUnpackRow<A>
{
    Iterator<A> iteratorFrom (Connection c, Statement s, ResultSet r);
}
