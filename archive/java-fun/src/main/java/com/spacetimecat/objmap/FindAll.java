package com.spacetimecat.objmap;

import com.spacetimecat.collection.Iterator;
import com.spacetimecat.function.BasicCheckedFunction1;

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
     * @param use the iterator is only valid inside this function
     * @param <R> the return type of {@code use} and this method
     * @return whatever returned by the function {@code use}
     */
    <R> R findAll (String keyColumn, Object key, BasicCheckedFunction1<? super Iterator<T>, R> use);
}
