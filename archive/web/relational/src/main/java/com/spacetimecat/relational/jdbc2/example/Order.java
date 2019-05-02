package com.spacetimecat.relational.jdbc2.example;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Order
{
    int value ();
}
