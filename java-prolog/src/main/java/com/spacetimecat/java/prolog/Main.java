package com.spacetimecat.java.prolog;

public final class Main {

    private Main () {}

    public static void main (String[] args) throws Throwable {

        System.out.println("Test");

        Term aa = Terms.atom("a");
        Term ab = Terms.atom("b");
        Term ac = Terms.atom("c");
        Term i1 = Terms.integer(1);
        Term i2 = Terms.integer(2);
        Term i3 = Terms.integer(3);
        Term v1 = Terms.var();
        Term v2 = Terms.var();
        Term v3 = Terms.var();
        Term s1 = Terms.array(i1, i2, i3);
        Term s2 = Terms.array(i1, i2, i3);
        Term s3 = Terms.array(v1, v2, v3);

        System.out.println("v1 = " + v1);
        System.out.println("v2 = " + v2);
        System.out.println("v3 = " + v3);
        System.out.println("s3 = " + s3);
        Unification_Imp u = new Unification_Imp();
        System.out.println(u.unify(
            Terms.array(
                aa, ab, ac
            ),
            Terms.array(
                v1, v2, v3
            )
        ));
        System.out.println("v1 = " + v1);
        System.out.println("v2 = " + v2);
        System.out.println("v3 = " + v3);
        System.out.println("s3 = " + s3);
        u.undo();
        System.out.println("v1 = " + v1);
        System.out.println("v2 = " + v2);
        System.out.println("v3 = " + v3);
        System.out.println("s3 = " + s3);

        System.out.println("s1 = " + s1);
        System.out.println("s2 = " + s2);
        System.out.println("s3 = " + s3);

    }

}
