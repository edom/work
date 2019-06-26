package com.spacetimecat.java.prolog;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.HashMap;

/**
 * <p>
 * Each instance of this class represents a Prolog term.
 * </p>
 * <p>
 * For consumers:
 * To instantiate this class, call the static factory methods in {@link Terms}.
 * </p>
 * <p>
 * For suppliers:
 * Remember these when adding a subclass:
 * </p>
 * <ul>
 * <li>Implement unification for the subclass in {@link Unification_Imp#unify(Term, Term)}.</li>
 * <li>Implement equality checking in {@link #equal(Term, Term)}.</li>
 * <li>
 * Override {@link #equals(Object)} and {@link #hashCode()}.
 * The difference between {@link #equals(Object)} and {@link #equal(Term, Term)}
 * is that the former is for Java {@link HashMap} and the latter is for Prolog {@code ==/2}.
 * They are not the same thing.
 * In particular, {@link #equals(Object)} does not {@link #dereference()} its arguments.
 * </li>
 * <li>Override {@link #to_java_object()}, if it makes sense for the subclass.</li>
 * <li>Override {@link #toString()}.</li>
 * </ul>
 */
public class Term {

    protected Term () {
    }

    // -------------------- Var

    private static final int DEFAULT_DEREFERENCE_LIMIT = Integer.MAX_VALUE;

    public final Term dereference () {
        return dereference(DEFAULT_DEREFERENCE_LIMIT);
    }

    public Object to_java_object () {
        throw new Prolog_Exception("Cannot convert from " + getClass() + " to Object");
    }

    /**
     * Return either an unbound Var or a non-Var.
     */
    public final Term dereference (int limit) {
        return dereference(0, limit);
    }

    protected Term dereference (int current, int limit) {
        return this;
    }

    /**
     * For quick testing.
     * Not for writing data for reading back later.
     */
    @Override
    public String toString () {
        return super.toString();
    }

    static boolean equal (Term a, Term b) {
        Term x = a.dereference();
        Term y = b.dereference();

        //  Here we are going to call the method equal_case(T(x), T(y))
        //  where T(x) is the exact class of x at runtime.
        //  Is this faster than a chain of instanceof checks?
        final MethodHandle method;
        try {
            MethodType type = MethodType.methodType(
                boolean.class
              , new Class<?>[] { x.getClass(), y.getClass() }
            );
            //  Should the object returned by lookup() be cached?
            method = MethodHandles.lookup().findStatic(Term.class, "equal_case", type);
        } catch (NoSuchMethodException e) {
            return false;
        } catch (IllegalAccessException e) {
            throw new AssertionError(e);
        }

        try {
            return (boolean) method.invoke(x, y);
        } catch (Error e) {
            throw e;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        } catch (RuntimeException e) {
            throw e;
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    static boolean equal_array (Term[] a, Term[] b) {
        if (a.length != b.length) {
            return false;
        }
        int n = a.length;
        for (int i = 0; i < n; ++i) {
            if (!equal(a[i], b[i])) {
                return false;
            }
        }
        return true;
    }

    static boolean equal_case (Term_Var a, Term_Var b) {
        return a == b;
    }

    static boolean equal_case (Term_Integer a, Term_Integer b) {
        return a.value.equals(b.value);
    }

    static boolean equal_case (Term_Atom a, Term_Atom b) {
        return a.string.equals(b.string);
    }

    static boolean equal_case (Term_Compound a, Term_Compound b) {
        return equal(a.name, b.name)
            && equal_array(a.args, b.args);
    }

    static boolean equal_case (Term_Array a, Term_Array b) {
        return equal_array(a.array, b.array);
    }

}
