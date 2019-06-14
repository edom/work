package com.spacetimecat.java.prolog.example;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import com.spacetimecat.java.prolog.Call;
import com.spacetimecat.java.prolog.Calls;
import com.spacetimecat.java.prolog.Term;
import com.spacetimecat.java.prolog.Terms;

public final class Example {

    private final PrintStream out = System.out;

    @Test
    public void terms () {
        out.println("-------------------- terms");
        out.println(Terms.var());
        out.println(Terms.atom("a"));
        out.println(Terms.integer(1));
        out.println(Terms.array(Terms.atom("a"), Terms.integer(1)));
        out.println(Terms.compound("f", Terms.atom("a"), Terms.integer(1)));
    }

    @Test
    public void backtracking () {
        //  ?- member(V, [a,b,c]).
        out.println("-------------------- backtracking");
        Term v = Terms.var();
        Term aa = Terms.atom("a");
        Term ab = Terms.atom("b");
        Term ac = Terms.atom("c");
        Call c = Calls.member(v, aa, ab, ac);
        List<Term> actual = new ArrayList<>();
        List<Term> expected = Arrays.asList(aa, ab, ac);
        while (c.next()) {
            actual.add(v.dereference());
            out.println("v = " + v + " ; ");
        }
        assertEquals(expected, actual);
    }

    @Test
    public void backtracking_conjunction () {
        //  ?- member(V1, [a,b,c]), member(V2, [1,2,3]).
        out.println("-------------------- backtracking conjunction");
        Term v1 = Terms.var();
        Term v2 = Terms.var();
        Term aa = Terms.atom("a");
        Term ab = Terms.atom("b");
        Term ac = Terms.atom("c");
        Term i1 = Terms.integer(1);
        Term i2 = Terms.integer(2);
        Term i3 = Terms.integer(3);
        Call c = Calls.and(
            Calls.member(v1, aa, ab, ac)
          , Calls.member(v2, i1, i2, i3)
        );
        while (c.next()) {
            out.println("v1 = " + v1 + ", v2 = " + v2 + " ; ");
        }
    }

    @Test
    public void backtracking_conjunction_with_unification () {
        //  ?- member(V1, [1,2,3]), member(V1, [1,2,3]).
        out.println("-------------------- backtracking conjunction with unification");
        Term v1 = Terms.var();
        Term i1 = Terms.integer(1);
        Term i2 = Terms.integer(2);
        Term i3 = Terms.integer(3);
        Call c = Calls.and(
            Calls.member(v1, i1, i2, i3)
          , Calls.member(v1, i1, i2, i3)
        );
        while (c.next()) {
            out.println("v1 = " + v1 + " ; ");
        }
    }

    @Test
    public void disjunctive_normal_form_1 () {
        /*
            ?-  member(V1, [a,b,c]), member(V2, [1,2,3])
            ;   member(V1, [1,2,3]), member(V2, [a,b,c]).
        */
        out.println("-------------------- disjunctive normal form 1");
        Term v1 = Terms.var();
        Term v2 = Terms.var();
        Term aa = Terms.atom("a");
        Term ab = Terms.atom("b");
        Term ac = Terms.atom("c");
        Term i1 = Terms.integer(1);
        Term i2 = Terms.integer(2);
        Term i3 = Terms.integer(3);
        Call c = Calls.or(
            Calls.and(
                Calls.member(v1, aa, ab, ac)
              , Calls.member(v2, i1, i2, i3)
            )
          , Calls.and(
                Calls.member(v1, i1, i2, i3)
              , Calls.member(v2, aa, ab, ac)
            )
        );
        while (c.next()) {
            out.println("v1 = " + v1 + ", v2 = " + v2 + " ; ");
        }
    }


    @Test
    public void disjunctive_normal_form_2 () {
        /*
            ?-  V = 1 ; V = 2 ; V = 3.
        */
        out.println("-------------------- disjunctive normal form 2");
        Term v = Terms.var();
        Term i1 = Terms.integer(1);
        Term i2 = Terms.integer(2);
        Term i3 = Terms.integer(3);
        Call c = Calls.or(
            Calls.unify(v, i1)
          , Calls.unify(v, i2)
          , Calls.unify(v, i3)
        );
        while (c.next()) {
            out.println("v = " + v + " ; ");
        }
    }

    @Test
    public void as_iterator () {
        out.println("-------------------- as iterator");
        Term v = Terms.var();
        Call c = Calls.member(v
          , Terms.atom("a")
          , Terms.integer(1)
          , Terms.atom("b")
          , Terms.integer(2)
        );
        Calls.each_1(v, c, j -> {
            out.println(j.getClass() + " " + j);
        });
    }

    @Test
    public void cut_1 () {
        out.println("-------------------- cut 1");
        Term v1 = Terms.var();
        Term v2 = Terms.var();
        Call c = Calls.frame(f ->
            Calls.and(
                Calls.member(v1
                  , Terms.integer(1)
                  , Terms.integer(2)
                  , Terms.integer(3)
                )
              , Calls.cut(f)
              , Calls.member(v2
                  , Terms.atom("a")
                  , Terms.atom("b")
                  , Terms.atom("c")
                )
            )
        );
        while (c.next()) {
            out.println("v1 = " + v1 + ", v2 = " + v2 + ";");
        }
    }

    @Test
    public void cut_2 () {
        out.println("-------------------- cut 2");
        Term v1 = Terms.var();
        Term v2 = Terms.var();
        Call c = Calls.and(
            Calls.frame(f ->
                Calls.and(
                    Calls.member(v1
                      , Terms.integer(1)
                      , Terms.integer(2)
                      , Terms.integer(3)
                    )
                  , Calls.cut(f)
                )
            )
          , Calls.member(v2
              , Terms.atom("a")
              , Terms.atom("b")
              , Terms.atom("c")
            )
        );
        while (c.next()) {
            out.println("v1 = " + v1 + ", v2 = " + v2 + ";");
        }
    }

    @Test
    public void cut_3 () {
        out.println("-------------------- cut 3");
        Term v1 = Terms.var();
        Term v2 = Terms.var();
        Call c = Calls.and(
            Calls.member(v1
              , Terms.integer(1)
              , Terms.integer(2)
              , Terms.integer(3)

            )
          , Calls.frame(f ->
                Calls.and(
                    Calls.member(v2
                      , Terms.atom("a")
                      , Terms.atom("b")
                      , Terms.atom("c")
                    )
                  , Calls.cut(f)
                )
            )
        );
        while (c.next()) {
            out.println("v1 = " + v1 + ", v2 = " + v2 + ";");
        }
    }

}
