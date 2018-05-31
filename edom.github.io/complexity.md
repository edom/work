---
title: Computational complexity
mathjax: yes
date: 2017-06-29 18:30 +0700
permalink: /complexity.html
---

- world effort
    - [A compendium of NP optimization problems](https://www.nada.kth.se/~viggo/wwwcompendium/wwwcompendium.html)
        - Smallest equivalent something:
https://en.wikipedia.org/wiki/Skeleton_(category_theory)
        - Minimum equivalent graph,
also called transitive reduction.
https://en.wikipedia.org/wiki/Transitive_reduction
https://www.nada.kth.se/~viggo/wwwcompendium/node49.html
    - [2013 Polymath project: Polymath 9: Discretized Borel Determinacy](https://polymathprojects.org/2013/11/04/polymath9-pnp/)
        - https://gowers.wordpress.com/2013/10/24/what-i-did-in-my-summer-holidays/
        - https://gowers.wordpress.com/2013/11/03/dbd1-initial-post/
    - https://cstheory.stackexchange.com/questions/4090/ways-for-a-mathematician-to-stay-informed-of-current-research-in-complexity-theo
- open access journals
    - [Logical methods in computer science](https://lmcs.episciences.org/browse/latest)
- recent publication trackers
    - arxiv list of recent submissions
        - [computer science](https://arxiv.org/list/cs/recent)
        - [computational complexity theory](https://arxiv.org/list/cs.CC/recent)
- NP-complete problems
    - [WP:List of NP-complete problems](https://en.wikipedia.org/wiki/List_of_NP-complete_problems)
    - https://mathoverflow.net/questions/72628/number-theory-and-np-complete
    - https://cstheory.stackexchange.com/questions/14124/is-there-a-natural-problem-on-the-naturals-that-is-np-complete
- descriptive complexity theory
    - Immerman and Vardi shows that FO(LFP) corresponds with P.
        - What does a FO(LFP) formula look like?
        - [Immerman--Vardi theorem](http://michaelnielsen.org/polymath1/index.php?title=Immerman-Vardi_theorem)
        - [FO(LFP) on Complexity Zoo](https://complexityzoo.uwaterloo.ca/Complexity_Zoo:F#folfp)
    - Fagin (?) proved that NP = ESO (existential second-order logic).
    - Immerman (?) proved that P = FO(LFP) (first-order logic with least fixed point).
        - Therefore, to prove that P does not equal NP,
        construct a sentence that is in ESO but not in FO(LFP).
            - Easier said than done?
                - Learn finite model theory?
                    - [Erich Grädel and Martin Grohe, "Is polynomial time choiceless?"](https://logic.rwth-aachen.de/~graedel/yurifest.pdf).
                    Choiceless polynomial time logic would imply \\( \TimeP \neq \TimeNP \\)?
                    - [Ronald Fagin's perspective on finite model theory](http://researcher.ibm.com/researcher/files/us-fagin/tcs93.pdf)
    - https://people.cs.umass.edu/~immerman/pub/ch0_1_2.pdf
    - [Descriptive Complexity and Language-Theoretic Complexity](http://www.gpwu.ac.jp/~satoru/lnlg05kuroda.pdf)
    by Satoru Kuroda
    - [Experimental descriptive complexity](https://people.cs.umass.edu/~immerman/pub/ExperimentalDC.pdf)
    by Neil Immerman et al
    - http://arxiv.org/pdf/cs/0409039v11.pdf
    On Certain Modular Equations
    Marius Constantin Ionescu
        - is this valid?
    - http://mathworld.wolfram.com/Computation.html
    - http://mathworld.wolfram.com/PrincipleofComputationalEquivalence.html
    - http://mathworld.wolfram.com/ComputationalIrreducibility.html
    - https://en.wikipedia.org/wiki/Algebraic_logic
- http://blog.computationalcomplexity.org/2010/05/structure-or-lack-thereof-of-data.html
- Plan for the P vs NP problem?
    - Relate configuration graph and problem theory
    - Unexplored ideas:
        - Machine is not computation.
        - Machine _is_ formal system.
        - Computation _is_ repeated function application.
        - *Under what conditions does nondeterminism give extra power?*
    - Where is computation theory, computability theory, complexity theory now?
- Complexity axioms
    - Blum's?
    - Here we axiomatize machine-independent *complexity*.
    - Recall that a problem \\( P \\) is a subset of <span>\( X \times Y \)</span>.
    We posit, without referring to any model of computation,
    that every question \\(x\\) has a *complexity* \\(m(x)\\), usually a number.
    The function \\( m \\) is a *complexity measure* of \\( P \\) iff it satisfies the axioms below.
    - For help, we define \\( S(k) \\)
    as the set of all questions with the same complexity \\( k \\),
    that is <span>\( S(k) = \{ x ~|~ m(x) = k \} \)</span>.
    Then, the *complexity axioms* are:
        - Every <span>\( S(k) \)</span> is finite.
        - There are always more complex questions than less complex questions,
        because, for example, longer strings can encode more questions.
        Formally, if \\( i < j \\) then <span>\( |S(i)| < |S(j)| \)</span>.
    - Note that the complexity measure is \\( m \\), not \\( S \\).
    - Computation graph axiom:
        - A machine can only manipulate one symbol at a time.
        Formally, if \\( E(a,b) \\) then <span>\( |m(a) - m(b)| \le 1 \)</span>.
        - The out-degree of a vertex of a nondeterministic graph may exceed 1 but cannot exceed a constant.
- Machine, algorithm, and complexity
    - The [time complexity](https://en.wikipedia.org/wiki/Worst-case_complexity#Definition)
    of machine \\(m\\) for input \\(x\\) is \\(t(m,x)\\),
    the number of steps \\(m\\) makes between the beginning and the halting.
    The _worst-case time complexity_ of \\(m\\) for input _size_ \\(n\\) is
    <span>\(T(m,n) = \left\vert \max_{|x| = n} t(m,x) \right\vert\)</span>.
    We can also write asymptotic statements such as \\(T(m,n) \in O(f(n))\\).
    - An algorithm \\(a\\) implies a machine \\(m(a)\\).
    - An _algorithm_ solves a _problem_.
    A problem can be solved by many algorithms with different resource usage characteristics.
    - The what (?) time complexity class of a problem is the worst-case time complexity of the most efficient algorithm solving that problem.
    - Machine _is_ algorithm.
    - A _machine_ \\(M\\) is a _transition relation_ \\(T\\)
    (an _acyclic_ binary relation).
\\[
T(x,y) = \text{\\(M\\) can state-transition from \\(x\\) to \\(y\\).}
\\]
    - \\(M\\) _computes_ \\(P\\) iff
    a subgraph of the shortcut of \\(T\\) is isomorphic to \\(P\\).
    (If \\(T\\) were cyclic, this definition would fail.)
    - Related:
    [graph isomorphism](https://en.wikipedia.org/wiki/Graph_isomorphism),
    [subgraph isomorphism problem](https://en.wikipedia.org/wiki/Subgraph_isomorphism_problem).
    - _Deterministic_ machine equals _functional_ relation.
    - \(G\) \emph{accepts} \(v\) iff \(F^\infty(\{v\}) = \emptyset\) where \(F\) is the graph's fringe function.
    The \emph{language} recognized by \(G\) is the largest \(L \subseteq V\) such that \(F^\infty(L) = \emptyset\).
    - A Turing machine is \((C,I,f)\)
    where \(C\) is countable
    and \(f\) is recursive.
    - https://en.wikipedia.org/wiki/Register_machine
    - Example: a state of a Turing machine is \((c,l,h,r)\)
    where \(c\) is a configuration,
    \(l\) is the tape content to the left of the head,
    \(h\) is the tape content at the head,
    and \(r\) is the tape content to the right of the head.
    - A problem class is a function.
    - A *problem* is a member of \(S\).
    - A *problem class* is a subset of \(S\).
    - Sometimes we can *reduce* a problem \(p : P\) into another problem \(q : Q\)
    by an injective reduction \(r : P \to Q\).
- Space and time complexity
    - Can we deal with complexity without ever defining machine and computation
    (besides assuming they exist)?
    - \(\newcommand\ftime{\text{time}}
    \newcommand\fspace{\text{space}}
    \newcommand\fsize{\text{size}}\)We are interested at the number of steps
    a machine makes for an input before terminating.
    We define \(\ftime~f~x\) as the *running time*
    of the machine \(f\) for input (initial state) \(x\).
    - We also define \(\fsize~x\) as the *size* of the state \(x\).
    - (Concrete)
    Formally, \(\ftime~f~x = n\) iff \(n\) is the smallest natural number such that \(f^{n+1}~x = f^n~x\).
    - We apply general algebraic thinking again, this time about congruences.
    Let \(T~f~n = \{ x ~|~ \ftime~f~x = n \} \)
    be the set of all inputs that \(f\) handles in \(n\) steps.
    We can also let \(U~n = \{ (f,x) ~|~ \ftime~f~x = n \}\)?
    \[
    \lambda f . \lambda n . \{ x ~|~ \ftime~f~x = n \}
    \\
    \lambda n . \{ (f,x) ~|~ \ftime~f~x = n \}
    \\
    \lambda m . \lambda n . \{ (f,x) ~|~ \fsize~x = m, ~ \ftime~f~x = n \}
    \]
    - We define \(\fspace : M~S \to S \to \Nat\),
    where \(\fspace~f~x\) is the size of the biggest state in \(\{ f^n~x ~|~ n \in \Nat \}\).
    - The other option is to require that each machine \(f\) have a \(t : S \to \Nat\) where \(t~(f~x) = 1 + t~x\).
- Complexity as an ordering of questions
    - Postulate:
    For every pair of questions, we can always decide which is more complex.
    Therefore, complexity is a *total ordering* of questions.
    We write \\( x < y \\) iff \\( x \\) is less complex than \\( y \\).
    Then the complexity axioms are:
        - For each \\( y \\), there are finitely many \\( x \\) such that \\( x \le y \\).
        - For each \\( x \\), there are infinitely many \\( y \\) such that \\( x \le y \\).
        (Is this required?)
    - A *reduction* \\( r \\) from problem \\( P \\) to problem \\( Q \\) is an order-preserving (but not necessarily order-reflecting) function
    that maps \\( P \\)-questions to \\( Q \\)-questions.
    Formally, for all \\( x, y \in P \\), if \\( x \le y \\) then \\( r(x) \le r(y) \\),
    but the converse does not need to hold.
    (Are we sure we don't need order-reflecting?)
    - Rabin complexity axioms?
    Still with the same \\( m \\) here.
    For help, let <span>\( L(k) = \{ x ~|~ m(x) \le k \} \)</span>
    be the set of all questions that are not more complex than \\( k \\).
    Then the axioms are:
        - Every \\( L(k) \\) is finite.
        - For every \\( i \\), there exists \\( j > i \\) such that \\( L(i) \subset L(j) \\).
        We say that \\( L \\) is *eventually increasing*.
        - The limit of \\( L(k) \\) as \\( k \\) grows unbounded is \\( X \\), the set of all questions.
        - (That is not what Rabin says? He uses Post canonical system and Curry-Howard correspondence?)
    - Corollary:
        - \\( L \\) is nondecreasing: If \\( i < j \\) then \\( L(i) \subseteq L(j) \\).
    - [WP:Order theory](https://en.wikipedia.org/wiki/Order_theory)
- Articles
    - [WP:Boolean satisfiability problem](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
    - [Where the really hard problems are](http://www.dcs.gla.ac.uk/~pat/cpM/papers/cheeseman91where.pdf), Cheeseman, Kanefsky, and Taylor, 
        - "Almost all k-colorable graphs are easy to color", J. S. Turner, 1988
    - Blum
        - [On the computational complexity of algorithms](https://www.researchgate.net/profile/Juris_Hartmanis/publication/242506038_On_the_Computational_Complexity_of_Algorithms/links/53fcd0a40cf2364ccc04db1d.pdf), J. Hartmanis and R. E. Stearns, 1965
    - [The complexity of total order structures](http://www.sciencedirect.com/science/article/pii/0022000078900089), Dan Moore, 1978
- Books
    - [Computational Complexity: A Modern Approach, by Sanjeev Arora and Boaz Barak](http://theory.cs.princeton.edu/complexity/), more than 400 pages, no finite model theory
    - [Introduction to Theory of Computation, by Anil Maheshwari and Michiel Smid](http://cglab.ca/~michiel/TheoryOfComputation/TheoryOfComputation.pdf), more than 200 pages
- [Blum 1976 "a machine independent theory of the complexity of recursive functions"](http://port70.net/~nsz/articles/classic/blum_complexity_1976.pdf), 15 pages
    - [Michael O. Rabin's 1960 technical report](https://www.cs.toronto.edu/~sacook/homepage/rabin_thesis.pdf)
        - [from cstheory stackexchange](https://cstheory.stackexchange.com/questions/34236/rabins-degree-of-difficulty-of-computing-a-function-and-a-partial-ordering-of)
- https://en.wikipedia.org/wiki/Proof_complexity
- A question related to P vs NP
    - Proving lower bound is much harder than proving upper bound.
    - Unsolved problem: How do we prove that an algorithm is the fastest solution of a problem?
    In order to prove that an algorithm is the fastest,
    it suffices us to prove that there is no faster algorithm for the same problem,
    but this is easier said than done.
        - https://cs.stackexchange.com/questions/38357/is-it-really-possible-to-prove-lower-bounds
- https://en.wikipedia.org/wiki/Time_hierarchy_theorem
- https://en.wikipedia.org/wiki/Constructible_function
- Entertainment
    - http://beza1e1.tuxen.de/articles/accidentally_turing_complete.html
