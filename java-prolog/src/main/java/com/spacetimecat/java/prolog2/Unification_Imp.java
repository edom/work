package com.spacetimecat.java.prolog2;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.invoke.MethodType;
import java.util.ArrayList;
import java.util.List;

final class Unification_Imp implements Unification {

    private final List<Runnable> undos = new ArrayList<>();

    void undo_to (int old_size) {
        for (int i = undos.size() - 1; i >= old_size; --i) {
            undos.remove(i).run();
        }
        assert undos.size() == old_size;
    }

    int size () {
        return undos.size();
    }

    void undo () {
        undo_to(0);
    }

    private void set (Var x, Object y) {
        Object old_referent = x.referent;
        undos.add(() -> { x.referent = old_referent; });
        x.referent = y;
    }

    @Override
    public boolean unify (Object a, Object b) {
        Object x = dereference(a);
        Object y = dereference(b);

        if (x == y) {
            return true;
        }
        if (x.equals(y)) {
            return true;
        }

        if (x instanceof Var) {
            set((Var) x, y);
            return true;
        }
        if (y instanceof Var) {
            set((Var) y, x);
            return true;
        }

        final String METHOD_NAME = "unify";
        final Lookup lookup = MethodHandles.lookup();
        final Class<?> cx = x.getClass();
        final Class<?> cy = y.getClass();
        final int old_size = undos.size();

        {
            MethodType type = MethodType.methodType(boolean.class, Unification.class, cy);
            try {
                MethodHandle method = lookup.findVirtual(cx, METHOD_NAME, type);
                if ((boolean) method.invoke(x, this, y)) {
                    return true;
                }
                undo_to(old_size);
                return false;
            } catch (NoSuchMethodException e) {
            } catch (Throwable e) {
                throw unchecked(e);
            }
        }

        //  If the method is not found, we try flipping the arguments.

        {
            MethodType type = MethodType.methodType(boolean.class, Unification.class, cx);
            try {
                MethodHandle method = lookup.findVirtual(cy, METHOD_NAME, type);
                if ((boolean) method.invoke(y, this, x)) {
                    return true;
                }
                undo_to(old_size);
                return false;
            } catch (NoSuchMethodException e) {
            } catch (Throwable e) {
                throw unchecked(e);
            }
        }

        return false;
    }

    @Override
    public boolean unify_array (Object[] a, Object[] b) {
        if (a.length != b.length) {
            return false;
        }
        int n = a.length;
        for (int i = 0; i < n; ++i) {
            if (!unify(a[i], b[i])) {
                return false;
            }
        }
        return true;
    }

    //  The return value is not used.
    static Error unchecked (Throwable e) {
        if (e instanceof Error) {
            throw (Error) e;
        }
        if (e instanceof RuntimeException) {
            throw (RuntimeException) e;
        }
        throw new RuntimeException(e);
    }

    static Object dereference (Object object) {
        if (object instanceof Var) {
            return ((Var) object).dereference_or_this();
        }
        return object;
    }

}
