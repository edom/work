package com.spacetimecat.java.prolog;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * <p>
 * This is like embedding Prolog in Java.
 * </p>
 * <p>
 * If you do not want to type the {@code Calls} prefix too much, use static imports.
 * </p>
 */
public final class Calls {

    private Calls () {}

    public static Call false_ () {
        return new Call_false();
    }

    public static Call true_ () {
        return new Call_true();
    }

    public static Call repeat () {
        return new Call_repeat();
    }

    /**
     * \+/1, negation as failure.
     */
    public static Call not (Call call) {
        return new Call_not(call);
    }

    public static Call and (Call... calls) {
        return new Call_and(calls);
    }

    public static Call or (Call... calls) {
        return new Call_or(calls);
    }

    public static Call once (Call call) {
        return new Call_once(call);
    }

    /**
     * =/2.
     */
    public static Call unify (Term a, Term b) {
        return new Call_unify(a, b);
    }

    /**
     * ==/2.
     */
    public static Call equal (Term a, Term b) {
        return new Call_equal(a, b);
    }

    public static Call member (Term elem, Term... list) {
        return new Call_member(elem, list);
    }

    /**
     * Delimit a {@linkplain #cut(Frame) cut}.
     */
    public static Call frame (Function<Frame, Call> make_call) {
        Frame frame = new Frame();
        Call call = make_call.apply(frame);
        return new Call_A() {
            @Override
            protected void do_reset () {
                frame.cut = false;
                call.reset();
            }
            @Override
            protected boolean do_next () {
                if (frame.cut) {
                    return false;
                }
                return call.next();
            }
        };
    }

    public static Call cut (Frame frame) {
        return new Call_cut(frame);
    }

    // cond(C,T,F) = (C -> T ; F)
    public static Call cond (Call c, Call t, Call f) {
        return new Call() {

            boolean c_called = false;
            Call tf = null;

            @Override
            public void reset () {
                c_called = false;
                tf = null;
                c.reset();
                t.reset();
                f.reset();
            }

            @Override
            public boolean next () {
                if (!c_called) {
                    c_called = true;
                    tf = c.next() ? t : f;
                }
                return tf.next();
            }

        };
    }

    public static void each_1 (Term var, Call call, Consumer<Object> consumer) {
        while (call.next()) {
            consumer.accept(var.to_java_object());
        }
    }

}
