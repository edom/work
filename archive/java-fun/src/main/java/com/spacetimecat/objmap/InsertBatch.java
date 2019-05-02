package com.spacetimecat.objmap;

import com.spacetimecat.collection.FiniteIterable;

public interface InsertBatch<T>
{
    void insert (FiniteIterable<T> list);
}
