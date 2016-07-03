package com.spacetimecat.objmap;

/**
 * <p>Each instance of this can generate the "SELECT column-list FROM table" prefix part of a SELECT statement.</p>
 */
public interface SqlSelectPrefix
{
    String sqlSelectPrefix ();
}
