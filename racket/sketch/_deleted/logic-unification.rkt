#lang racket/base

;;  --------------------    Unification.

(require
    (only-in racket/list
        cons?
        empty
    )
)

(require "logic-var.rkt")

(module+ unification-1
    ;;  successful? : Bool
    ;;  victims : Listof Var
    (struct Unification (successful? victims) #:transparent #:mutable)

    (define (undo! unification)
        (unset-all! (Unification-victims unification))
    )

    ;;  unify! : Any Any -> Unification
    (define (unify! A B)
        (define victims empty) ;; List of modified variables.
        (define (go A B)
            (define C (dereference A))
            (define D (dereference B))
            (cond
                ;;  Both C and D refer to the same thing.
                ((eq? C D)
                    #t
                )

                ;;  Make sure that Vars are leftmost,
                ;;  to simplify the other cases.
                ((and (not (Var? C)) (Var? D))
                    (go D C)
                )

                ((unbound-Var? C)
                    (set! victims (cons C victims))
                    (bind! C D)
                    #t
                )

                ((and (vector? C) (vector? D))
                    (and
                        (= (vector-length C) (vector-length D))
                        (for/and (
                                (E (in-vector C))
                                (F (in-vector D))
                            )
                            (go E F)
                        )
                    )
                )

                ((and (cons? C) (cons? D))
                    (and
                        (go (car C) (car D))
                        (go (cdr C) (cdr D))
                    )
                )

                (else
                    (equal? C D)
                )
            )
        )
        (Unification (go A B) victims)
    )

    ;;  --------------------    Example.

    (printf "==================== Example 1~n")
    (define A (fresh))
    (define B (fresh))
    (define C '(a b))
    (printf "-------------------- before unification~n")
    (printf "~a~n" A)
    (printf "~a~n" B)
    (printf "~a~n" C)
    (define U (unify! (list A B) C))
    (printf "-------------------- unification result~n")
    (Unification-successful? U)
    (Unification-victims U)
    (printf "-------------------- after unification~n")
    (printf "~a~n" A)
    (printf "~a~n" B)
    (printf "~a~n" C)
    (printf "-------------------- after reset~n")
    (undo! U)
    (printf "~a~n" A)
    (printf "~a~n" B)
    (printf "~a~n" C)
)

(module+ unification-2
    ;;  Call = Any ... Found -> Step Close
    ;;  Found = Bool -> Any
    ;;  Step = -> Any (find next unification)
    ;;  Close = -> Any

    (define tag (make-continuation-prompt-tag 'unification))

    ;;  Succeed = -> Any
    ;;  Fail = -> Any
    ;;  unify! : Any Any Succeed Fail -> Step Close
    (define (unify! A B succeed fail)
        ;;  List of modified variables.
        (define hostages empty)
        (define (kidnap! victim)
            (set! hostages (cons victim hostages))
        )
        (define (release-all-hostages!)
            (unset-all! hostages)
            (set! hostages empty)
        )
        ;;  Return a boolean indicating whether unification was successful.
        ;;  Variables may be clobbered on failure.
        (define (go A B)
            (define C (dereference A))
            (define D (dereference B))
            (cond
                ;;  Both C and D refer to the same thing.
                ((eq? C D)
                    #t
                )
                ;;  Make sure that Vars are leftmost,
                ;;  to simplify the other cases.
                ((and (not (Var? C)) (Var? D))
                    (go D C)
                )
                ((unbound-Var? C)
                    (kidnap! C)
                    (bind! C D)
                    #t
                )
                ((and (vector? C) (vector? D))
                    (and
                        (= (vector-length C) (vector-length D))
                        (for/and (
                                (E (in-vector C))
                                (F (in-vector D))
                            )
                            (go E F)
                        )
                    )
                )
                ((and (cons? C) (cons? D))
                    (and
                        (go (car C) (car D))
                        (go (cdr C) (cdr D))
                    )
                )
                (else
                    (equal? C D)
                )
            )
        )
        (define state 0)
        (define (step)
            (case state
                ((0)
                    (set! state 1)
                    (if (go A B)
                        (succeed)
                        (begin
                            (release-all-hostages!)
                            (fail)
                        )
                    )
                )
                ((1)
                    (set! state 2)
                    (release-all-hostages!)
                    (fail)
                )
                (else
                    (fail)
                )
            )
        )
        (define (close)
            (set! state 2)
            (release-all-hostages!)
        )
        (values step close)
    )

    (define ((%= A B) succeed fail)
        (unify! A B succeed fail)
    )

    (define ((%and G1 G2) succeed fail)
        (define-values (step1 close1) (G1 succeed1 fail1))
        (define-values (step2 close2) (G2 succeed2 fail2))
        (define (step)
            (step1)
            (step2)
        )
        (define (close)
            (close2)
            (close1)
        )
        (values step close)
    )

    ;;  --------------------    Example.

    (printf "==================== Example 2~n")
    (define A (fresh))
    (define B (fresh))
    (define C '(a b))
    (printf "-------------------- before unification~n")
    A B C
    (define-values (step close)
        (unify! (list A B) C
            (lambda () #t)
            (lambda () #f)
        )
    )
    (printf "-------------------- unification result~n")
    (step)
    A B C
    (printf "-------------------- step again~n")
    (step)
    A B C
    (printf "-------------------- after reset~n")
    (close)
    A B C
)
