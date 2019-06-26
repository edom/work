package com.spacetimecat.java.prolog2;

import java.io.PrintStream;

import com.spacetimecat.java.prolog.Call;
import com.spacetimecat.java.prolog.Call.Callback.Action;

public final class Main {

    private Main () {}

    public static void main (String[] args) {
        PrintStream out = System.out;
        Var v1 = new Var();
        Var v2 = new Var();
        Compound a = new Compound("f", v1, v2);
        Compound b = new Compound("f", "a", 1);
        out.println("A = " + a + ", B = " + b + " ; ");
        out.println("--------------------");
        {
            Call c = Calls2.unify(a, b);
            c.each(() -> {
                out.println("A = " + a + ", B = " + b + " ; ");
                return Action.REDO;
            });
        }
        out.println("--------------------");
        out.println("A = " + a + ", B = " + b + " ; ");
        out.println("--------------------");
        Calls2.and(
            Calls2.member(v1, "a", "b"),
            Calls2.member(v2, 1, 2)
        ).each(() -> {
            out.println("X = " + v1 + ", Y = " + v2 + " ; ");
            return Action.REDO;
        });
        out.println("--------------------");
        out.println("X = " + v1 + ", Y = " + v2 + " ; ");
        out.println("--------------------");
        {
            Compound com = new Compound("f", "a", 1, new Compound("g", 2, 3), new Var());
            {
                Call c = Calls2.and(
                    Calls2.unify(v2, 1),
                    Calls2.arg(v1, com, v2)
                );
                c.each(() -> {
                    out.println("X = " + v1 + ", Y = " + v2 + " ; ");
                    return Action.REDO;
                });
            }
        }
        out.println("-------------------- Database");
        {
            Database d = new Database();
            d.assertz_fact(new Compound("father", "abraham", "isaac"));
            d.assertz_fact(new Compound("father", "abraham", "ishmael"));
            d.assertz_fact(new Compound("mother", "sarah", "isaac"));
            d.assertz_fact(new Compound("mother", "hagar", "ishmael"));
            {
                // male(A) :- father(A, _).
                Var v = new Var();
                d.assertz_clause(new Compound("male", v), new Compound("father", v, new Var()));
            }
            {
                // husband_wife_child(H,W,C) :- father(H,C), mother(W,C).
                Var h = new Var();
                Var w = new Var();
                Var c = new Var();
                d.assertz_clause(
                    new Compound("husband_wife_child", h, w, c),
                    comma(
                        new Compound("father", h, c),
                        new Compound("mother", w, c)
                    )
                );
            }
            {
                Var father = new Var();
                Var mother = new Var();
                Var child = new Var();
                Compound goal =
                    comma(
                        new Compound("father", father, child),
                        new Compound("mother", mother, child)
                    );
                d.prove(goal).each(() -> {
                    System.out.println(goal);
                    return Action.REDO;
                });
            }
            {
                Var v = new Var();
                Compound goal = new Compound("male", v);
                d.prove(goal).each(() -> {
                    System.out.println(goal);
                    return Action.REDO;
                });
            }
            {
                Var h = new Var();
                Var w = new Var();
                Var c = new Var();
                Compound goal = new Compound("husband_wife_child", h, w, c);
                d.prove(goal).each(() -> {
                    System.out.println(goal);
                    return Action.REDO;
                });
            }
        }
        out.println("-------------------- copy_term");
        {
            Var x = new Var();
            Var y = new Var();
            Var z = new Var();
            Object com = new Compound("f", x, y, z, x, y, z);
            Object cop = Database.copy(com);
            out.println(com);
            out.println(cop);
        }
    }

    private static Compound comma (Object a, Object b) {
        return new Compound(",", a, b);
    }

    private static Compound semicolon (Object a, Object b) {
        return new Compound(";", a, b);
    }

}
