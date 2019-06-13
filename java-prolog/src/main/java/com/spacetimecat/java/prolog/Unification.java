package com.spacetimecat.java.prolog;

import java.util.ArrayList;
import java.util.List;

final class Unification {

    //  This is a collection of Runnable in order to implement b_setarg/3.

    private final List<Runnable> restores = new ArrayList<>();

    public Unification () {
    }

    /**
     * <p>
     * First-order unification without occurs-check.
     * </p>
     * <p>
     * If the unification fails, all bindings done so far are undone.
     * The effect is as if this method was never called at all.
     * </p>
     */
    public boolean unify (Term a, Term b) {
        if (unify_clobber(a, b)) {
            return true;
        }
        undo();
        return false;
    }

    private boolean unify_clobber (Term a, Term b) {

        Term x = a.dereference();
        Term y = b.dereference();

        if (x == y) {
            return true;
        }

        if (x instanceof Term_Var) {
            return unify_var((Term_Var) x, y);
        }
        if (y instanceof Term_Var) {
            return unify_var((Term_Var) y, x);
        }

        if (x instanceof Term_Compound) {
            if (y instanceof Term_Compound) {
                return unify_compound((Term_Compound) x, (Term_Compound) y);
            }
            return false;
        }
        if (y instanceof Term_Compound) {
            return false;
        }

        if (x instanceof Term_Array) {
            if (y instanceof Term_Array) {
                return unify_array((Term_Array) x, (Term_Array) y);
            }
            return false;
        }
        if (y instanceof Term_Array) {
            return false;
        }

        return x.equals(y);

    }

    private boolean unify_var (Term_Var var, Term term) {
        if (var.referent != null) {
            //  "var.referent" must be null because unify has called "dereference".
            throw new AssertionError();
        }
        restores.add(() -> { var.referent = null; });
        var.referent = term;
        return true;
    }

    private boolean unify_compound (Term_Compound a, Term_Compound b) {
        return
            unify(a.name, b.name)
         && unify_array_0(a.args, b.args);
    }

    private boolean unify_array (Term_Array a, Term_Array b) {
        return unify_array_0(a.array, b.array);
    }

    private boolean unify_array_0 (Term[] x, Term[] y) {
        if (x.length != y.length) {
            return false;
        }
        int n = x.length;
        for (int i = 0; i < n; ++i) {
            if (!unify(x[i], y[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * Pretend that this unification never happened.
     */
    public void undo () {
        //  The restores must be run in the reverse order they are added.
        for (int i = restores.size() - 1; i >= 0; --i) {
            restores.get(i).run();
        }
        restores.clear();
    }

}
