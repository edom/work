public class Hello
{
    public static void main (String[] args)
    {
        for (int i = 0; i < 5; ++i)
        {
            System.out.println("hello");
        }
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
}
