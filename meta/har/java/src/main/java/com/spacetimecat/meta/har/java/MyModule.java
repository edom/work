package com.spacetimecat.meta.har.java;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import javax.inject.Named;
import javax.sql.DataSource;

final class MyModule extends AbstractModule {
    @Override
    protected void configure () {
    }

    @Provides
    @Named("test")
    DataSource ds_test () {
        final HikariConfig c = new HikariConfig();
        c.setJdbcUrl("jdbc:postgresql://localhost/test");
        c.setUsername("test");
        c.setPassword("test");
        c.setMaximumPoolSize(8);
        return new HikariDataSource(c);
    }
}
