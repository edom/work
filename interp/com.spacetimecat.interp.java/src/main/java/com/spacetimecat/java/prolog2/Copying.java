package com.spacetimecat.java.prolog2;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.HashMap;
import java.util.Map;

import com.spacetimecat.java.prolog.Prolog_Exception;

final class Copying {

    final Map<Object, Object> concordance = new HashMap<>();

    Object copy (Object a) {
        Object d = Unification_Imp.dereference(a);
        Class<?> cls = d.getClass();
        try {
            MethodHandle method = MethodHandles.lookup()
                .findVirtual(Copying.class, "copy_case", MethodType.methodType(cls, cls));
            return method.invoke(this, d);
        } catch (NoSuchMethodException e) {
            throw new Prolog_Exception("Don't know how to copy an instance of " + cls, e);
        } catch (Throwable e) {
            throw Unification_Imp.unchecked(e);
        }
    }

    Object[] copy_array (Object[] a) {
        int n = a.length;
        Object[] b = new Object[n];
        for (int i = 0; i < n; i++) {
            b[i] = copy(a[i]);
        }
        return b;
    }

    Var copy_case (Var a) {
        return (Var) concordance.computeIfAbsent(a, k -> new Var());
    }

    Compound copy_case (Compound a) {
        return new Compound(copy(a.name), copy_array(a.args));
    }

    Integer copy_case (Integer a) {
        return a;
    }

    String copy_case (String a) {
        return a;
    }

    Clause copy_case (Clause a) {
        return new Clause(copy(a.head), copy(a.body));
    }

}