/**
 * <p>
 *     JDBC usability improvement.
 * </p>
 *
 * <p>
 *     This library is very opinionated but straightforwardly interoperates
 *     with legacy code that uses standard JDBC.
 *     If there is something that you can't do with this library,
 *     you can use JDBC directly.
 * </p>
 *
 * <h2>Scenarios</h2>
 *
 * <ul>
 *     <li>
 *         A programmer wants to deserialize a row in a {@link java.sql.ResultSet}
 *         to an instance of a Java interface.
 *     </li>
 *     <li>
 *         A programmer wants to generate a table from a Java interface
 *         that describes the table.
 *     </li>
 * </ul>
 *
 * <h2>Deserializing rows from {@link java.sql.ResultSet}</h2>
 *
 * <p>
 *     The basic idea: calling the method {@code getAbc} should
 *     return the value associated with the key {@code PAbc}
 *     where {@code P} is a prefix specified by the client programmer.
 * </p>
 *
 * <p>
 *     Every row can be seen as a map with string keys.
 *     The class {@link com.spacetimecat.relational.jdbc2.ResultSetMap2} does this.
 * </p>
 *
 * <p>
 *     Every object can also be seen as a map with string keys.
 *     The class {@link com.spacetimecat.relational.dyno.DynamicObject} does this.
 * </p>
 *
 *
 * <h3>Design of the system</h3>
 *
 * <p>
 *     The client programmer provides an interface {@code I} and a map {@code M}.
 *     The system will provide a proxy instance of {@code I} such that the method {@code getX}
 *     returns the value associated with the key {@code X} in {@code M}.
 * </p>
 * <p>
 *     {@link com.spacetimecat.relational.jdbc2.RowIterator#read(java.lang.String, java.lang.Class)}.
 * </p>
 * <p>
 *     Cannot be used with generic types (types having type parameters).
 * </p>
 * <p>
 *     Null cells are forbidden. Decompose your tables further, or separate your queries.
 * </p>
 *
 * <p>
 *     The client programmer provides a map {@code M}.
 *     The system will provide a map {@code N} such that {@code M[PK] = N[K]}.
 *     This can be done like this: {@code (k) -> M.get(p + k)}.
 * </p>
 *
 * <h2>Subpackages</h2>
 *
 * <ul>
 *     <li>
 *         Unchecked exceptions.
 *     </li>
 *     <li>
 *         Continuation-passing-style methods automatically close resources.
 *     </li>
 * </ul>
 *
 * <h3>The SQL-to-Java part of the ORM</h3>
 *
 * <ul>
 *     <li>
 *         Given a table-describing interface,
 *         this implements that interface
 *         by reading from a {@link java.sql.ResultSet}.
 *     </li>
 * </ul>
 *
 * <p>
 *     Automatically implement an interface whose getters
 *     will read the columns of a {@link java.sql.ResultSet} row.
 * </p>
 *
 * <h2>Principles</h2>
 *
 * <h3>You must always use all columns of the tables in your query</h3>
 *
 * <p>
 *     If you find yourself wanting to select some but not all columns,
 *     your design is wrong.
 *     You have two options to fix it:
 * </p>
 *
 * <ul>
 *     <li>Split your SQL table.</li>
 *     <li>Split your Java interface.</li>
 * </ul>
 *
 * <p>
 *     Supporting partial queries would necessitate
 *     returning nulls from getters.
 *     We don't want getters to return nulls.
 * </p>
 *
 * <h3>Nulls</h3>
 *
 * <p>
 *     If your column can have null value, use a reference type.
 *     If your getter return type is a primitive type,
 *     and the column is null,
 *     there will be a {@link java.lang.NullPointerException}.
 * </p>
 *
 * <h3>Column name case-sensitivity is unspecified</h3>
 *
 * <p>
 *     If your getter is named {@code getAbcDef},
 *     then your column should be named {@code AbcDef}.
 * </p>
 *
 * <h3>{@link com.spacetimecat.relational.jdbc2.sync}: The Java-to-SQL part of the ORM</h3>
 *
 * <ul>
 *     <li>
 *         Check whether a table is compatible
 *         with the corresponding table-describing interface.
 *     </li>
 *     <li>
 *         Generate CREATE TABLE statements
 *         from table-describing interfaces.
 *     </li>
 * </ul>
 *
 * <ul>
 *     <li>
 *         {@link com.spacetimecat.relational.jdbc2.query}:
 *         An embedded Domain Specific Language for relational algebra
 *         that can be translated to SQL.
 *         However, we recommend that you write your SQL as plain text
 *         (without using this package, or any other DSL)
 *         and use only widely-supported ANSI SQL features.
 *     </li>
 * </ul>
 */
package com.spacetimecat.relational.jdbc2;
