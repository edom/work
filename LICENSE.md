- Definitions:
  - A _license file_ is a file whose name begins with `LICENSE`.
  - A _file-system node_ is either a directory, a file, or a link.
    (Note that those three categories are disjoint.)
  - The _parent_ of a file-system node N is the directory that directly contains N.
  - The _referent_ of a link is what the link refers to.
- Procedure: To determine the license of a file-system node N in this repository,
  pick the first of these clauses whose condition is satisfied by N:
  - If N is a directory that contains a license file, then license(N) = that license.
  - If N is a file that contains a license, then license(N) = that license.
  - If N is a link, then license(N) = license(referent(N)).
  - Otherwise, license(N) = license(parent(N)).

The default license (the license of the directory that directly contains this `LICENSE.md` file)
is any of the following licenses, of which you may choose zero or more:
- [Apache-2.0](https://www.apache.org/licenses/LICENSE-2.0)
- [BSD-2-clause](https://spdx.org/licenses/BSD-2-Clause.html)
- [MIT](https://spdx.org/licenses/MIT.html)

This license file itself is in public domain
(or is licensed under [CC0-1.0](https://creativecommons.org/publicdomain/zero/1.0/legalcode.txt)
if your jurisdiction is too complicated).
You can freely copy and modify this license file for your own works.

Everything in this repository comes without any warranties.
Use at your own risk.
