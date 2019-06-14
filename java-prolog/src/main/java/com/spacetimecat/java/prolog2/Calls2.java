package com.spacetimecat.java.prolog2;

import java.util.function.BiFunction;
import java.util.function.Function;

import com.spacetimecat.java.prolog.Call;
import com.spacetimecat.java.prolog.Calls;
import com.spacetimecat.java.prolog.Prolog_Exception;

/**
 * <p>
 * Higher-order abstract syntax embedding of Prolog in Java.
 * </p>
 */
public final class Calls2 {

    private Calls2 () {}

    public static Call and (Call... calls) {
        return Calls.and(calls);
    }

    public static Call or (Call... calls) {
        return Calls.or(calls);
    }

    public static Call not (Call call) {
        return Calls.not(call);
    }

    public static Call exists (Function<Var, Call> make) {
        return make.apply(new Var());
    }

    public static Call exists (BiFunction<Var, Var, Call> make) {
        return make.apply(new Var(), new Var());
    }

    @FunctionalInterface
    interface Semidet {
        boolean call (Unification u);
    }

    static Call semidet (Semidet imp) {
        return new Call() {
            final Unification_Imp u = new Unification_Imp();

            boolean done = false;

            @Override
            public void reset () {
                done = false;
                u.undo();
            }

            @Override
            public boolean next () {
                u.undo();
                if (done) {
                    return false;
                }
                done = true;
                return imp.call(u);
            }
        };
    }

    public static Call unify (Object a, Object b) {
        return semidet(u -> u.unify(a, b));
    }

    public static Call equal (Object a, Object b) {
        return semidet(u -> a.equals(b));
    }

    public static Call member (Object elem, Object... list) {
        return new Call() {
            final Unification_Imp u = new Unification_Imp();
            int i = 0;

            @Override
            public void reset () {
                i = 0;
                u.undo();
            }

            @Override
            public void each (Callback c) {
                u.undo();
                while (i < list.length) {
                    if (u.unify(elem, list[i++])) {
                        switch (c.call()) {
                            case REDO: u.undo(); break;
                            case STOP: return;
                        }
                    }
                }
            }
        };
    }

    public static Call arg (Object index, Object compound, Object arg) {
        return new Call() {
            final Unification_Imp u = new Unification_Imp();
            int ix = 0;

            @Override
            public void reset () {
                ix = 0;
                u.undo();
            }

            @Override
            public boolean next () {
                u.undo();
                Object dc = dereference(compound);
                if (dc instanceof Var) {
                    throw new Prolog_Exception("instantiation error");
                }
                if (dc instanceof Compound) {
                    Compound c = (Compound) dc;
                    Object di = dereference(index);
                    int n = c.args.length;
                    while (ix < n) {
                        if (di instanceof Integer) {
                            ix = n;
                            int i = ((Integer) di) - 1;
                            return 0 <= i && i < n && u.unify(c.args[i], arg);
                        }
                        if (u.unify(c.args[ix++], arg)) {
                            if (u.unify(index, Integer.valueOf(ix))) {
                                return true;
                            }
                            u.undo();
                        }
                    }
                }
                return false;
            }
        };
    }

    public static Call var (Object a) {
        return semidet(u -> dereference(a) instanceof Var);
    }

    public static Call nonvar (Object a) {
        return not(var(a));
    }

    private static Object dereference (Object a) {
        return Unification_Imp.dereference(a);
    }

}
