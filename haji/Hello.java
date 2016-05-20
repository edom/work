public class Hello
{
    private static int g = 5;

    public static void main (String[] args)
    {
        for (int i = 0; i < 5; ++i)
        {
            System.out.println("hello");
        }
    }

    public static int test (int a, int b)
    {
        System.out.println(g);
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
