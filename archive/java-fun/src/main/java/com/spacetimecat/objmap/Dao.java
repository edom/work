package com.spacetimecat.objmap;

import com.spacetimecat.objmap.annotation.Field;

import javax.sql.DataSource;

/**
 * <p>A deserializer based on reflection.</p>
 *
 * <p>Use {@link Daos#of(Class)} to obtain an instance of this.</p>
 *
 * @see Field
 */
public interface Dao<T>
{
    /**
     * <p>Calling this method does not immediately make any connection to the database.
     * This just constructs a representation of something that can connect to the database.</p>
     * @param db the whereabouts of the database
     * @return a representation
     */
    ConnectedDao<T> connect (DataSource db);
}
