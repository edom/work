package com.spacetimecat.web;

/**
 * Select matching XML elements.
 *
 * @author erik
 */
public interface Cursor
{

    Cursor next_sibling ();

    Cursor first_child ();

    // Element element ();

}
