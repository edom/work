---
title: PostgreSQL
permalink: /pgsql.html
date: 2018-05-15 02:31:00 +0700
---

- Questions
    - How do we find slow queries?
    - Which queries lock some tables or the database?
    - Is there a GUI or visualization tool?
        - There should already be a tool for visualizing such Postgresql statistics?
    - Is autovacuum enabled on our RDS clusters? How do we know?
    - Do we need [reliability](http://stackoverflow.com/questions/19135340/how-does-postgresql-perform-writes-so-much-faster-than-sqlite)?
    If we can sacrifice reliability, we can speed things up by turning fsync off.
    - How much performance does RDS encryption cost?
    [Negligible, but with caveats.](http://blog.minjar.com/post/108724853340/rds-encryption-and-benchmarking-postgresql)
- Answers looking for questions
    - There is a [statistics collector](https://www.postgresql.org/docs/9.4/static/monitoring-stats.html).
    - [Logging Difficult Queries](https://wiki.postgresql.org/wiki/Logging_Difficult_Queries)
    - [Postgresql needs regular maintenance](https://www.postgresql.org/docs/9.4/static/maintenance.html).
    One critical maintenance task is to VACUUM the database;
    the query planner relies on VACUUM ANALYZE.
    - [Turning PostgreSQL into a queue serving 10,000 jobs per second](https://gist.github.com/chanks/7585810)
    - [Towards 14,000 write transactions on a laptop](http://pgeoghegan.blogspot.co.id/2012/06/towards-14000-write-transactions-on-my.html)
    - [How many updates per second can a standard RDBMS process?](https://blogs.harvard.edu/philg/2011/01/10/how-many-updates-per-second-can-a-standard-rdbms-process/)
- PostgreSQL tuning
    - http://madusudanan.com/blog/understanding-postgres-caching-in-depth/#CachePurpose
    - https://wiki.postgresql.org/wiki/Logging_Difficult_Queries
