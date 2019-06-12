package com.spacetimecat.java.prolog.example;

import static org.junit.jupiter.api.Assertions.*;

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
        Call c1 = Calls.member(v1, aa, ab, ac);
        Call c2 = Calls.member(v2, i1, i2, i3);
        Call c = Calls.and(c1, c2);
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
        Call c1 = Calls.member(v1, i1, i2, i3);
        Call c2 = Calls.member(v1, i1, i2, i3);
        Call c = Calls.and(c1, c2);
        while (c.next()) {
            out.println("v1 = " + v1 + " ; ");
        }
    }

    @Test
    public void disjunctive_normal_form () {
        /*
            ?-  member(V1, [a,b,c]), member(V2, [1,2,3])
            ;   member(V1, [1,2,3]), member(V2, [a,b,c]).

        */
        out.println("-------------------- disjunctive normal form");
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

}
