import java.util.ArrayList;
import java.util.List;

public class Hello
{

    private static long getLong ()
    {
        return 3345L;
    }

    static class V0 { public long m () { return 1L; } }
    static class V1 extends V0 { }

    static boolean invokevirtual_0 ()
    {
        final V0 v = new V1();
        return v.m() == 1L;
    }

    static boolean invokevirtual_1 ()
    {
        final V1 v = new V1();
        return v.m() == 1L;
    }

    static class V2 { public long m () { return 2L; } }
    static class V3 extends V2 { public long m () { return 3L; } }

    static boolean invokevirtual_2 ()
    {
        final V2 v = new V3();
        return v.m() == 3L;
    }

    static boolean invokevirtual_3 ()
    {
        final V3 v = new V3();
        return v.m() == 3L;
    }

    // private static native void println (int x);
    private static native void println (long x);
    private static native void println (Object x);
    // private static native void println (String x);

    // private static void println (long x) { System.out.println(x); }

    public static void main (String[] args)
    {
        main();
    }

    public static void main ()
    {
        assert invokevirtual_0();
        assert invokevirtual_1();
        assert invokevirtual_2();
        assert invokevirtual_3();
        /*
        final List<Integer> a = new ArrayList<>();
        a.add(0);
        a.add(1);
        a.add(2);
        println(a);
        */
        // println(System.nanoTime());
        // println(123);
        // println(getLong());
        // new Hello();
        // println("Hello world");
        // System.out.println("hello world");
        // System.out.println("hello world");
        // new Hello().add(0L, 1L);
    }

    public Hello ()
    {
        final C0 a = new C0(1234567);
        println(a.x);
    }

    public class C0 { public long x; public C0 (long x) { this.x = x; } }
    // public static class C1 extends C0 {}
    // public static class C2 extends C1 {}

    public long add (long a, long b)
    {
        return a + b;
    }

    // private static int g = 5;
    private static long g;
    {
        g = 213L;
    }

    public static int test (int a, int b)
    {
        if (true) { return 0; }
        int i = 0;
        b = test2(1, 0);
        while (a > 0)
        {
            i += b;
            --a;
        }
        return i;
    }

    public static int test2 (int a, int b)
    {
        int r = test3(a);
        for (int i = 0; i < 10; ++i)
        {
            r -= b;
        }
        return r;
    }

    public static int test3 (int a)
    {
        return a + 10;
    }

    public static int test_switch (int i)
    {
        switch (i)
        {
            case 0:
                return 1;
            case 1:
                return 2;
            case 2:
                return 3;
            default:
                return 0;
        }
    }

    public static int call_this_from_haskell (int i, int j)
    {
        return 100 * i;
    }
}
