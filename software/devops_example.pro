:- module(devops_example, [
]).

:- use_module('./devops.pro', [setup/0]).

/*
Model the part of our network that we are interested in.

I call my machine 'dev'.
Host name here doesn't have to coincide with actual host names.
*/

devops:pov_host_address_distance(P,H,A,D) :- pov_host_address(P,H,A,D).
pov_host_address_distance(dev, jump, '1.2.3.4', 1).
pov_host_address_distance(dev, jump1, '1.2.3.5', 1).
pov_host_address_distance(jump, app, '10.0.0.128', 1).
pov_host_address_distance(jump, db, '10.0.0.129', 1).
pov_host_address_distance(jump, jump2, '172.16.0.8', 1).
% jump1 is a backup of jump.
pov_host_address_distance(jump1, H, A, D) :- pov_host_address(jump, H, A, D).
% jump2, app2 are at different network.
pov_host_address_distance(jump2, app2, '172.16.0.128', 1).

want(dev, 2022, jump, 22).
want(dev, 2023, app, 22).
want(dev, 2024, app2, 22).
want(dev, 5434, db, 5432).
want(dev, 3000, jump2, 22).
