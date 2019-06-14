package com.spacetimecat.java.prolog2;

public interface Unification {

    /**
     * <p>
     * This method tries these in order:
     * </p>
     * <p>Let x = dereference(a). (That is, {@link #dereference(Object)}.)</p>
     * <p>Let y = dereference(b).</p>
     * <ul>
     * <li>Return true if x == y or x.equals(y).</li>
     * <li>
     * Return x.unify(this,y) if that method exists.
     * The method must be an instance method in class Cx
     * and have the shape {@code boolean unify (Unification, Cy)}.
     * </li>
     * <li>Return y.unify(this,x) if that method exists.</li>
     * <li>Return false.</li>
     * </ul>
     * <p>
     * If x.unify/y.unify returns false,
     * than this undoes all partial unification done by x.unify/y.unify since this method was called,
     * and the state of this object is restored to the state before this method was called.
     * </p>
     */
    boolean unify (Object a, Object b);

    boolean unify_array (Object[] a, Object[] b);

}
