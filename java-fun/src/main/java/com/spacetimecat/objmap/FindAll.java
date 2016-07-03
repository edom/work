package com.spacetimecat.objmap;

import com.spacetimecat.collection.RowIterator;

import javax.sql.DataSource;

/**
 * <p>This is part of {@link ConnectedDao}.</p>
 *
 * <p>You can get this from {@link Dao#connect(DataSource)}.</p>
 */
public interface FindAll<T>
{
    /**
     * <p>This SELECTs all rows from the underlying table whose 'keyColumn' equals 'key'.</p>
     * @param keyColumn name of the column in the WHERE clause
     * @param key the other side of the equality expression in the WHERE clause
     * @return a view that has to be closed later
     */
    RowIterator<T> findAll (String keyColumn, Object key);
}
