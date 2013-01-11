package id.web.edom.rmi;

import java.util.Arrays;

public class MethodDescription
{
    private final Class returnType;
    private final String name;
    private final Class[] parameterTypes;
    public MethodDescription (Class returnType, String name, Class[] parameterTypes)
    {
        this.returnType = canonicalize(returnType);
        this.name = name;
        this.parameterTypes = new Class[parameterTypes.length];
        for (int i = 0; i < parameterTypes.length; ++i)
        {
            this.parameterTypes[i] = canonicalize(parameterTypes[i]);
        }
    }
    private Class canonicalize (Class c)
    {
        if (c.equals(boolean.class)) return Boolean.class;
        if (c.equals(void.class)) return Void.class;
        if (c.equals(char.class)) return Character.class;
        if (c.equals(byte.class)) return Byte.class;
        if (c.equals(short.class)) return Short.class;
        if (c.equals(int.class)) return Integer.class;
        if (c.equals(long.class)) return Long.class;
        if (c.equals(float.class)) return Float.class;
        if (c.equals(double.class)) return Double.class;
        return c;
    }
    public Class getReturnType () { return returnType; }
    public String getName () { return name; }
    public Class[] getParameterTypes () { return parameterTypes; }
    public boolean equals (Object obj)
    {
        if (!(obj instanceof MethodDescription)) return false;
        final MethodDescription that = (MethodDescription) obj;
        return this.returnType.equals(that.returnType) && this.name.equals(that.name)
                && Arrays.equals(this.parameterTypes, that.parameterTypes);
    }
    public int hashCode ()
    {
        return returnType.hashCode() + name.hashCode() + Arrays.hashCode(parameterTypes);
    }
}
