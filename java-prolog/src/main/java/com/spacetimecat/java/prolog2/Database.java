package com.spacetimecat.java.prolog2;

import static com.spacetimecat.java.prolog.Calls.cond;
import static com.spacetimecat.java.prolog.Calls.false_;
import static com.spacetimecat.java.prolog.Calls.true_;
import static com.spacetimecat.java.prolog2.Calls2.and;
import static com.spacetimecat.java.prolog2.Calls2.equal;
import static com.spacetimecat.java.prolog2.Calls2.exists;
import static com.spacetimecat.java.prolog2.Calls2.or;
import static com.spacetimecat.java.prolog2.Calls2.unify;
import static com.spacetimecat.java.prolog2.Calls2.var;

import java.util.ArrayList;
import java.util.List;

import com.spacetimecat.java.prolog.Call;
import com.spacetimecat.java.prolog.Calls;
import com.spacetimecat.java.prolog.Prolog_Exception;
import com.spacetimecat.java.prolog.Call.Callback.Action;

final class Database {

    public static final Compound FALSE = new Compound("false");
    public static final Compound TRUE = new Compound("true");

    final List<Clause> clauses = new ArrayList<>();

    void assertz_fact (Object fact) {
        assertz_clause(fact, TRUE);
    }

    void assertz_clause (Object head, Object body) {
        clauses.add(new Clause(head, body));
    }

    Call clause (Object head, Object body) {
        return new Call() {
            Unification_Imp u = new Unification_Imp();
            int i = 0;

            @Override
            public void reset () {
                i = 0;
                u.undo();
            }

            @Override
            public boolean next () {
                u.undo();
                while (i < clauses.size()) {
                    Clause c = clauses.get(i++);
                    if (u.unify(head, c.head) && u.unify(body, c.body)) {
                        return true;
                    }
                    u.undo();
                }
                return false;
            }
        };
    }

    static Object copy (Object a) {
        return new Copying().copy(a);
    }

    Call prove (Object goal) {
        /*
        return
            cond(equal(goal, TRUE), true_(),
                cond(equal(goal, FALSE), false_(),
                    cond(var(goal), false_(),
                        exists((v,w) ->
                            cond(unify(new Compound(",", v, w), goal),
                                and(prove(v), prove(w)),
                                cond(unify(new Compound(";", v, w), goal),
                                    or(prove(v), prove(w)),
                                    exists(body ->
                                        and(
                                            clause(goal, body),
                                            prove(body)
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            );
        */
        return new Call() {
            Unification_Imp u = new Unification_Imp();
            Call next;

            @Override
            public void reset () {
                if (next != null) {
                    next.reset();
                    next = null;
                }
                u.undo();
            }

            @Override
            public boolean next () {
                if (next == null) {
                    Object d = Unification_Imp.dereference(goal);
                    if (!(d instanceof Compound)) {
                        throw new Prolog_Exception("Not a compound: " + d);
                    }
                    Compound c = (Compound) d;
                    if (u.unify(c, TRUE)) {
                        next = true_();
                    } else if (u.unify(c, FALSE)) {
                        next = false_();
                    } else {
                        Var v = new Var();
                        Var w = new Var();
                        if (u.unify(c, new Compound(",", v, w))) {
                            next = and(prove(v), prove(w));
                        } else if (u.unify(c, new Compound(";", v, w))) {
                            next = or(prove(v), prove(w));
                        } else {
                            next = and(clause(c, v), prove(v));
                        }
                    }
                }
                return next.next();
            }
        };
    }

}
