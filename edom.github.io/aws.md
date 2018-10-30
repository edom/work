---
title: Amazon Web Services
permalink: /aws.html
date: 2018-05-15 02:31:00 +0700
---

# Amazon Web Services

- How do we detect if we're running on AWS?
    - [Question on AWS forum](https://forums.aws.amazon.com/message.jspa?messageID=122425)
    - Some choices:
        - on instance launch time: set an environment variable in the AMI used to launch instances.
        This seems to be the most reliable way.
        - on application runtime:
            - HTTP server at 169.254.169.254
            - Reverse DNS lookup
            - `/proc/xen` (if your development machine doesn't use xen)
- How do we get EC2 instance metadata?
    - http://stackoverflow.com/questions/625644/find-out-the-instance-id-from-within-an-ec2-machine
    - http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html
- Amazon RDS is not for scaling.
It is designed to simplify operation of relational databases.
[It is not designed to scale relational databases horizontally](https://www.quora.com/Does-Amazon-RDS-solve-the-MySQL-scaling-issue).
- The write capacity does not raise in proportion to the number of machines.
- Deployment with Amazon Machine Images
    - We assume that the code scales horizontally.
    - Install everything you need to that instance.
    - Snapshot an AMI from an instance.
- EC2 security notes:
    - Because all instances are launched from the same image,
    they have the same SSH host keys.
    Compromising any of them will also compromise all other instances sharing the key.
    - See also [Amazon's notes on building shared AMIs](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/building-shared-amis.html).
- How do I install AWS CLI on Ubuntu 14.04?
    - `sudo apt-get install awscli`
- RDS
    - In Amazon RDS PostgreSQL, slow queries are not logged by default.
    See [RDS user guide](http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.Concepts.PostgreSQL.html).
    - [Using Route 53 for aliasing your RDS instances](http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-rds-db.html)
    - [RDS best practices](http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_BestPractices.html)
- Shit did happen.
    - A busy RDS instance got CPU-throttled (ran out of CPU credits).
        - *CPU credit doesn't have to reach zero* in order for the instance to be throttled.
        Don't use CloudWatch alarm condition `CpuCredit = 0`.
    - A busy RDS instance got IOPS-throttled (ran out of IOPS credits).
        - 2018 CloudWatch doesn't have IOPS credit metric.
        Can't make alarm.
- [AWS Device Farm](https://aws.amazon.com/blogs/aws/aws-device-farm-update-remote-access-to-devices-for-interactive-testing/): interactive testing on real devices.
