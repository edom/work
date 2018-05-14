---
title: Logging
permalink: /logging.html
date: 2018-05-15 02:31:00 +0700
---

- Before logging something, think how it would be read.
- If you can't read the log, you shouldn't write the log.
- Only log important things.
Keep logging volume low.
Minimize retention; only retain important logs.
Don't retain debugging logs.
- Storage is cheap. Rotate your logs and buy more storage.
- If you're looking for things to do, maybe you can try using Amazon SDK
and log your events to CloudWatch or CloudTrail (or perhaps even SNS).
Treat a log as an event stream, not a file.
- http://jasonwilder.com/blog/2013/07/16/centralized-logging-architecture/
- http://jasonwilder.com/blog/2012/01/03/centralized-logging/
