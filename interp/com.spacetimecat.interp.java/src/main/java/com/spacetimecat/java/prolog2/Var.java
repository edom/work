package com.spacetimecat.java.prolog2;

import java.util.Map;
import java.util.Set;

/**
 * <p>
 * Because an instance of this class is mutable,
 * it should not be in a {@link Set}
 * and should not be used as a {@link Map} key.
 * </p>
 */
public final class Var {

    Object referent;

    public Object dereference_or_this () {
        if (referent == null) {
            return this;
        }
        if (referent instanceof Var) {
            return ((Var) referent).dereference_or_this();
        }
        return referent;
    }

    @Override
    public String toString () {
        if (referent == null) {
            return String.format("Var@%x", System.identityHashCode(this));
        }
        return referent.toString();
    }

}
