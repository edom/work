#lang racket/base

(require
    racket/match
    racket/set
    racket/pretty
)

(provide
    main
)

;;  The core language is lambda calculus with some atoms/constants.
;;
;;      ('con val)
;;      ('var sym)
;;      ('lam par bod)
;;      ('app fun arg)
;;      ('if)
;;      ('if cnd)
;;      ('if cnd tru)
;;      ('if cnd tru fal)
;;      ('prim-app)
;;      ('prim-app proc)
;;      ('prim-app proc arg)
;;
;;  Compare:
;;
;;      -   lambda calculus
;;      -   GHC Core https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/core-syn-type

(define (exp? thing)
    (match thing
        [(list 'con _) #t]
        [(list 'var sym) (symbol? sym)]
        [(list 'lam par bod) (and (symbol? par) (exp? bod))]
        [(list 'app fun arg) (and (exp? fun) (exp? arg))]
        [_ #f]))

;;  Exp -> Listof Symbol
(define (free-vars-in exp)
    (define (go bvs exp)
        (define (bound? name) (member name bvs))
        (match exp
            [(list 'var name)       (if (bound? name) '() (list name))]
            [(list 'app fun arg)    (append (go bvs fun) (go bvs arg))]
            [(list 'lam par bod)    (go (cons par bvs) bod)]
            [(list 'con _)          '()]
            [(list 'if)             '()]
            [(list 'println)        '()]
            [(list 'prim-app pro arg) (append (go bvs pro) (go bvs arg))]
            [(? procedure?)         '()]
            ))
    (list-unique (go '() exp)))

(define (list-unique lst) (set->list (list->set lst)))

(define (to-racket-value exp)
    (match exp
        [(list 'con val) val]
        [_ (raise-argument-error 'to-racket-value "a con" 0 exp)]))

;;  2019-11-01
;;  Constraints:
;;      -   exp must not have any free variables.
;;  Advantages:
;;      -   Does not require a context parameter.
;;  Drawbacks:
;;      -   Inefficient.
(define (normalize-by-substitution exp)
    (define free-vars (free-vars-in exp))
    (unless (null? free-vars)
        (raise-arguments-error 'normalize-by-substitution "expression contains free variables"
            "expression" exp
            "free variables" free-vars))
    ;;  Normalize a lambda expression.
    (define (nor exp)
        (match exp
            [(list 'con _)          exp]
            [(list 'var _)          exp]
            [(list 'lam _ _)        exp]
            [(list 'app fun arg)    (app fun arg)]
            ;;  Isn't this prim-app too powerful?
            [(list 'prim-app pro arg) (pro arg)]
            [(list 'if cnd tru fal) (nor (if (equal? (nor cnd) (list 'con #f))
                                             fal
                                             tru))]
            [(list 'println sub)    (list 'con (println (to-racket-value (nor sub))))]
            [_                      (raise-argument-error 'exp "a lambda expression" 0 exp)]))
    ;;  Normalize an application ('app fun arg).
    (define (app fun arg)
        (match fun
            [(list 'lam par bod)    (nor (subst par arg bod))]
            [(list 'app f a)        (app (app f a) arg)]
            [(list 'println)        (list 'println arg)]
            [(list 'if)             (list 'if arg)]
            [(list 'if a)           (list 'if a arg)]
            [(list 'if a b)         (nor (list 'if a b arg))]
            [(list 'prim-app)       (list 'prim-app arg)]
            [(list 'prim-app pro)   (nor (list 'prim-app pro arg))]
            [_                      (raise-arguments-error 'apply "cannot apply"
                                        "function" fun "argument" arg)]))
    ;;  Substitute all free occurrence of ('var sym) in exp with rep.
    (define (subst sym rep exp)
        (define (sub exp)
            (match exp
                [(list 'var var)        (if (equal? sym var) rep exp)]
                [(list 'con _)          exp]
                [(list 'app fun arg)    (list 'app (sub fun) (sub arg))]
                [(list 'lam par bod)    (if (equal? sym par) exp (list 'lam par (sub bod)))]
                [_                      (raise-arguments-error 'subst "cannot replace" "name" sym "with" rep "in" exp)]))
        (sub exp))
    (nor exp))

;;  Scheme-like language with currying.
;;
;;  In this language, lam is a reserved keyword.

;;  (lam (x y ...) bod) -> (lam x (lam y ...) bod')
;;  (x y z ...)         -> (app (app (app x' y') z') ...)
;;  symbol              -> (var symbol)
;;  other               -> (con other)
(define (desugar exp)
    (define (app head args)
        (match args
            [(list)             head]
            [(cons arg0 rest)   (app (list 'app head (desugar arg0)) rest)]))
    (define (lam pars bod)
        (match pars
            [(list)             (desugar bod)]
            [(cons par0 rest)   (if (equal? par0 'lam)
                                    (error 'desugar "lambda parameter name cannot be reserved keyword 'lam")
                                    (list 'lam par0 (lam rest bod)))]))
    (define (deslet bnds body)
        (match bnds
            [(list)
                (desugar body)]
            [(cons (list nam exp) rest)
                (list 'app
                    (list 'lam nam (deslet rest body))
                    (desugar exp))]))
    (match exp
        [(? symbol?)                                (list 'var exp)]
        [(list 'prim thing)                         (list thing)]
        [(list 'quote thing)                        (list 'con thing)]
        [(list 'let bnds body)                      (deslet bnds body)]
        [(list 'lam pars bod) #:when (list? pars)   (lam pars bod)]
        [(cons 'lam _)                              (raise-argument-error 'desugar "bad lambda form" "(lam Pars Body)" exp)]
        [(cons head args)                           (app (desugar head) args)]
        [_                                          (list 'con exp)]))

(define (main)
    (define exp
        `[(lam (x y) (x y))
            (lam (x) x)
            123
        ])
    #;(define exp `[println (if #t 0 1)])
    #;(define exp `[if #t (lam (x) x) (lam (x) 0) 123])
    ;;  Why can't lam be as first-class as if?
    ;;  If we want lam to be first-class, we have to sacrifice the separation between desugar and normalize?
    #;(define exp `[(lam (conditional) (conditional #t 0 1)) if])
    #;(define exp `[let [(x 0) (y 1)] x])
    ;;  Linking.
    (define lex `[(lam (if println) ,exp) (prim if) (prim println)])
    (define dex (desugar lex))
    #;(define dex
        `(app
            (app
                (lam x (lam y (app (var x) (var y))))
                (lam x (var x)))
            (con 123)))
    (pretty-print exp)
    (pretty-print lex)
    (pretty-print dex)
    (define rex
        `[prim-app ,(λ (x) x) (con 1)])
    (pretty-print rex)
    (pretty-print (normalize-by-substitution rex)))
