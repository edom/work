/** <module> DevOps

Don't use this.
Just join the tunnels in your ~/.ssh_config file into one Host.

Facts/configurations:
    - pov_host_address_distance/4
    - ssh_forward/4

Actions:
    - setup/0

Queries:
    - origin_destination_path_distance/4
    - origin_destination_shortest_path_distance/4

For guided example, see devops_example.pro.
*/
:- module(devops, [
    setup/0
    , pov_host_address_distance/4
    , ssh_forward/4
    , origin_destination_path_distance/4
]).

/** setup

Establish all SSH tunnels according to ssh_forward/4.

We want to generate a sequence of SSH commands.

Each SSH command has the shape =|ssh -N -L<L1> -L<L2> ... -R<R1> -R<R2> ... Host|=.

We model each SSH command with a term ssh(Host, LocFwds, RemFwds).
*/
setup :- fail.

/** pov_host_address_distance(?Pov, ?Host, ?Addr, ?Dist)

"From Pov's point of view, the address of Host is Addr, at distance Dist away from Pov."

Pov is a host name.
The term Pov stands for point-of-view.

Host is a host name.

Addr is an IPv4 address that Pov uses to reach Host via SSH.

The expression pov_host_address(Pov, Host, Addr) can be read as:
    - From Pov's point of view, the address of Host is Addr.
    - Pov sees that Host has address Addr.
    - Pov can vanilla-IPv4-SSH to Host at address Addr.

Dist is an integer.

If pov_host_address(Pov, Host, _, _) is not in the knowledge base,
then we assume that Pov can't reach Host directly.

Example:
pov_host_address(dev, jump, '1.2.3.4', 100).
*/
:- multifile pov_host_address_distance/4.

/** origin_destination_path_distance(?Ori, ?Dst, ?Path, -Dist) is nondet

Ori is a host name.

Dst is a host name.

Path is a list of some edges in the network graph.
    - Path is a list of A-B.
    - Path begins with Ori-Something.
    - Path ends with Something-Dst.

Dist is total distance of Path.
*/
origin_destination_path_distance(A, Z, [A-Z], D) :-
    pov_host_address_distance(A, Z, _, D).
origin_destination_path_distance(A, Z, [A-B | Ls], D) :-
    pov_host_address_distance(A, B, _, DAB),
    origin_destination_path_distance(B, Z, Ls, DLs),
    D is DAB + DLs.

origin_destination_shortest_path_distance(A, Z, P, D) :-
    findall(Dist-Path, origin_destination_path_distance(A, Z, Path, Dist), Pairs),
    sort(Pairs, [D-P | _]).

/** ssh_forward(?SrcHost, ?SrcTcpAddr, ?DstHost, ?DstTcpAddr)

"Whenever SrcHost receives a SrcTcpAddr-destined packet:
SrcHost shall forward it to DstHost,
and DstHost shall forward it to DstTcpAddr."

"Every SrcTcpAddr-destined TCP connection to SrcHost should be forwarded to DstTcpAddr via DstHost."

SrcTcpAddr is seen from SrcHost's point of view.

DstTcpAddr is seen from DstHost's point of view.

SrcTcpAddr and DstTcpAddr are passed to SSH LocalForward or RemoteForward options as described in =|man ssh_config|=.

SrcTcpAddr and DstTcpAddr has the shape AddrPort.
See example below.
    - AddrPort is a concatenation of Addr, a colon, and Port.
    - Addr may be an IPv4 atom like '1.2.3.4'.
    - Addr may be the atom '*' (only for SrcTcpAddr).
    - Port is an integer.

Example:
    - ssh_forward(dev, '*:2000', jump, '1.2.3.4:2000').
    - ssh_forward(dev, 'localhost:3000', jump, '2.3.4.5:4000').
*/
:- multifile ssh_forward/4.
