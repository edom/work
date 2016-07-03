/**
 * <p>Object mapping; see {@link com.spacetimecat.objmap.Daos}.</p>
 *
 * <p>This package provides ways to convert between an <em>unpacked representation</em>
 * and any of these <em>packed representations</em>:
 *
 * <ul>
 *     <li>{@link java.sql.ResultSet}</li>
 *     <li>{@code Map<String, Object>}</li>
 * </ul>
 *
 * <p>You only need to implement {@link com.spacetimecat.objmap.BasicUnpack}.</p>
 *
 * <h2>Unpacked representation</h2>
 *
 * <p>An unpacked representation is a Java class that has a constructor
 * whose parameters are annotated with {@link com.spacetimecat.objmap.annotation.Field}.</p>
 */
package com.spacetimecat.objmap;