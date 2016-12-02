package com.spacetimecat.relational.jdbc2.example;

import com.spacetimecat.relational.jdbc2.Database;
import com.spacetimecat.relational.jdbc2.DumpResultSet;
import com.spacetimecat.relational.jdbc2.query.Equal;
import com.spacetimecat.relational.jdbc2.query.Select;
import com.spacetimecat.relational.jdbc2.sync.TableDescribingInterface;
import org.h2.jdbcx.JdbcDataSource;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.SQLException;
import java.util.function.Function;

public final class Main
{
    public static void main (String... args) throws SQLException
    {
        final JdbcDataSource dataSource = new JdbcDataSource();
        dataSource.setUrl("jdbc:h2:/tmp/h2-test");
        final Database database = new Database(dataSource);
        database.execute("CREATE SCHEMA IF NOT EXISTS what");
        database.execute("CREATE TABLE what.department (id IDENTITY, name VARCHAR (256) NOT NULL, PRIMARY KEY (id))");
        database.execute("CREATE TABLE what.employee (id IDENTITY, DepartmentId BIGINT NOT NULL, name VARCHAR (256) NOT NULL, salary INTEGER NOT NULL, PRIMARY KEY (id), FOREIGN KEY (DepartmentId) REFERENCES what.department (id))");
        database.execute("CREATE TABLE what.supervise (supervisor BIGINT NOT NULL, supervisee BIGINT NOT NULL, PRIMARY KEY (supervisor, supervisee), UNIQUE KEY (supervisee), FOREIGN KEY (supervisor) REFERENCES what.employee (id), FOREIGN KEY (supervisee) REFERENCES what.employee (id))");
        database.execute("INSERT INTO what.department (name) VALUES ('Dept 1'), ('Dept 2')");
        database.execute("INSERT INTO what.employee (name, DepartmentId, salary) VALUES ('A', 1, 30000), ('B', 2, 70000), ('C', 1, 100000)");
        database.execute("INSERT INTO what.supervise VALUES (1, 2)");
        final String ddl = new TableDescribingInterface(EmployeeRow.class).getDataDefinition();
        System.out.println(ddl);
        final String sql = new Select(EmployeeRow.class, "what.employee", "e").toSqlSelect();
        System.out.println(sql);
        database.executeQueryC(sql, rows ->
        {
            while (rows.next())
            {
                final EmployeeRow e_ = rows.read("e_", EmployeeRow.class);
                System.out.println(e_.getId());
            }
//                new DumpResultSet(rows.unwrap()).to(System.out);
        });
        final String foo = new Select(EmployeeRow.class, "what.employee", "sup")
            .join(new Select(SuperviseRow.class, "what.supervise", "s"))
            .join(new Select(EmployeeRow.class, "what.employee", "sub"))
            .filter(new Equal("sup_Id", "s_Supervisor").and(new Equal("s_Supervisee", "sub_Id")))
            .toSqlSelect();
        System.out.println(foo);
        database.executeQueryC(foo, rows ->
            {
                while (rows.next())
                {
                    final EmployeeRow supervisor = rows.read("sup_", EmployeeRow.class);
                    final EmployeeRow subordinate = rows.read("sub_", EmployeeRow.class);
                    System.out.printf("%s %s supervises %s %s\n", supervisor.getId(), supervisor.getName(), subordinate.getId(), subordinate.getName());
                }
            }
        );
        final String foo2 = new Select(EmployeeRow.class, "what.employee", "e")
            .join(new Select(SuperviseRow.class, "what.supervise", "s"))
            .join(new Select(DepartmentRow.class, "what.department", "d"))
            .join(new Select(EmployeeRow.class, "what.employee", "f"))
            .filter(
                new Equal("e_DepartmentId", "d_Id")
                .and(new Equal("e_Id", "s_Supervisee"))
                .and(new Equal("s_Supervisor", "f_Id"))
            )
            .toSqlSelect();
        System.out.println(foo);
        database.executeQueryC("EXPLAIN " + foo2, rows -> new DumpResultSet(rows.unwrap()).to(System.out));
        database.executeQueryC(foo2, rows ->
        {
            while (rows.next())
            {
                final EmployeeRow row = rows.read("e_", EmployeeRow.class);
                final DepartmentRow dept = rows.read("d_", DepartmentRow.class);
                final EmployeeRow sup = rows.read("f_", EmployeeRow.class);
                final Employee employee = new Employee(row, sup);
                System.out.println(String.format("%s %s %s %s %s %s"
                    , employee.getId()
                    , employee.getName()
                    , dept.getId()
                    , dept.getName()
                    , sup.getId()
                    , sup.getName()));
            }
        });

    }

    public static <T> Method getReferencedMethod (Class<T> type, Function<T, ?> getterMethodRef)
    {
        final T proxy = (T) Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(), new Class[]{type}, new GetMethodInvocationHandler());
        try
        {
            getterMethodRef.apply(proxy);
            throw new AssertionError();
        }
        catch (GetMethodResult result)
        {
            return result.method;
        }
    }
}
