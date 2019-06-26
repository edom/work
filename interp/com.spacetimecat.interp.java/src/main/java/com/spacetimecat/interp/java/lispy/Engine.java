package com.spacetimecat.interp.java.lispy;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Verb-oriented programming?
 * Overloading?
 */
@SuppressWarnings({
    "rawtypes",
    "unchecked",
})
public final class Engine {

    public Object apply (Object obj, Object arg) {
        // break = goto error
        for (;;) {
            if (obj instanceof Function) {
                return ((Function) obj).apply(arg);
            }
            // An instance of Map can be thought of as a finite function.
            if (obj instanceof Map) {
                Object value = ((Map) obj).get(arg);
                if (value == null) {
                    return Error_Value.from("key not found");
                }
            }
            if (obj instanceof Consumer) {
                ((Consumer) obj).accept(arg);
                return Unit.get_instance();
            }
            if (obj instanceof Supplier) {
                return ((Supplier) obj).get();
            }
            if (obj instanceof List) {
                if (arg instanceof Number) {
                    return ((List) obj).get(((Number) arg).intValue());
                }
                break;
            }
            if (obj instanceof Set) {
                return ((Set) obj).contains(arg);
            }
            if (obj instanceof Iterable) {
                if (arg instanceof Number) {
                    return at((Iterable) obj, ((Number) arg).intValue());
                }
                break;
            }
            if (obj instanceof Object[]) {
                return ((Object[]) obj)[(int) arg];
            }
            break;
        }
// error:
        return Error_Value.from_new_RuntimeException(
            String.format("apply(%s, %s)", class_of(obj), class_of(arg))
        );
    }

    private static Object at (Iterable iter, int index) {
        if (index < 0) {
            throw new IndexOutOfBoundsException("" + index);
        }
        int i = index;
        for (Object elem : iter) {
            if (i <= 0) {
                return elem;
            }
            --i;
        }
        throw new IndexOutOfBoundsException("" + index);
    }

    public Object set (Object place, Object value) {
        if (place instanceof Place) {
            ((Place) place).set(value);
            return Unit.get_instance();
        }
        if (place instanceof Object[]) {
            ((Object[]) place)[0] = value;
            return Unit.get_instance();
        }
        if (place instanceof AtomicReference) {
            ((AtomicReference) place).set(value);
            return Unit.get_instance();
        }
        throw new UnsupportedOperationException(
            String.format("set(%s, %s)", class_of(place), class_of(value))
        );
    }

    /**
     * Similar to Scheme:
     * Only {@link Boolean#FALSE} is falsy,
     * and everything else is truthy.
     */
    public boolean is_truthy (Object a) {
        return Objects.equals(a, Boolean.FALSE);
    }

    private static Class class_of (Object x) {
        return x == null ? null : x.getClass();
    }

}
