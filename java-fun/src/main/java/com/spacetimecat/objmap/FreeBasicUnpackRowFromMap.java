package com.spacetimecat.objmap;

import com.spacetimecat.collection.BasicMap;
import com.spacetimecat.collection.BasicMaps;

import java.sql.ResultSet;

final class FreeBasicUnpackRowFromMap<A> implements BasicUnpackRow<A>
{
    private final BasicUnpack<BasicMap<String, Object>, A> bu;

    FreeBasicUnpackRowFromMap (BasicUnpack<BasicMap<String, Object>, A> bu)
    {
        this.bu = bu;
    }

    @Override
    public A unpack (ResultSet r)
    {
        final BasicMap<String, Object> bm = BasicMaps.from(r);
        return bu.unpack(bm);
    }
}
