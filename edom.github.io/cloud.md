---
title: Moving to clouds, for old-school sysadmins
permalink: /cloud.html
date: 2017-06-26 16:30:00 +0700
---

# Moving to clouds, for old-school sysadmins

The most important pages on the [AWS website](https://aws.amazon.com/)
are the pricing pages and the technical documentation.
That website has much content, but not much information,
perhaps because they are not selling to sysadmins.

<table>
<tr>
<th>Amazonese</th>
<th>Old-school</th>
</tr>
<tr>
<td>Route 53</td>
<td>managed DNS server</td>
</tr>
<tr>
<td>VPC (virtual private cluster)</td>
<td>managed LAN (local area network)</td>
</tr>
<tr>
<td>EC2 (elastic compute cloud) instance</td>
<td>managed virtual machine</td>
</tr>
<tr>
<td>security group</td>
<td>managed iptables/firewall</td>
</tr>
<tr>
<td>RDS (relational database service)</td>
<td>managed SQL server</td>
</tr>
<tr>
<td>EBS (elastic block store)</td>
<td>managed NAS (network-attached storage)</td>
</tr>
<tr>
<td>ELB (elastic load balancer)</td>
<td>managed HAProxy</td>
</tr>
<tr>
<td>ElastiCache</td>
<td>managed Memcached/Redis</td>
</tr>
<tr>
<td>Lambda</td>
<td>automatically turn on machines to run a piece of code,
and turn off idle machines</td>
</tr>
</table>

AWS, GCE, and Azure do the same thing you used to do.
The difference is they do it on a much larger scale,
and they make an API on top of it,
so you can _automate_ it,
but this also mean that _you_ can be automated away,
so beware!

With this cloud stuff, you can't buy a machine and bring it to the data center.
You start a machine from your computer.
The machine is now virtual;
it doesn't correspond to a motherboard anymore.
Procuring a machine is just a few clicks on the website,
or a few keystrokes on the terminal,
and your machine will be running in a few minutes.
What you used to do in days, now you can do in minutes.

With this cloud stuff, you can't visit the data center to restart a stuck machine.
You restart it from your computer.

You're billed per hour.
What was infrastructure (like roads) is now utility (like electricity).

The cloud is cheaper for bursty load with low average load.
If your average load is high, old-school is cheaper.

One thing doesn't change: you still need to back up data to a safe place _outside_ the cloud.
(I'm a hypocrite; I say that but I don't do that.)
