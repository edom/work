package com.spacetimecat.objmap;

public interface ConnectedDao<T> extends
    FindAll<T>
    , QueryRawSql<T>
{
}
