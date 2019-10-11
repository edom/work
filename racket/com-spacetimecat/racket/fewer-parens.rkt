#lang s-exp "lang.rkt"

;;  Forms with fewer parentheses.

(provide

    LET LET* LETREC LETREC*

    λ   ;;  This conflicts with Racket's λ.

    !=
    not-equal?

)

;;  --------------------    LET-like forms with fewer parentheses.

(define-syntax-parser _GLET
    #:datum-literals (IN)
    [(_ Let (~seq Name Expr) ... IN Body ...)
        #'(Let ((Name Expr) ...) Body ...)])

(define-syntax-rule (LET Arg ...) (_GLET let Arg ...))
(define-syntax-rule (LET* Arg ...) (_GLET let* Arg ...))
(define-syntax-rule (LETREC Arg ...) (_GLET letrec Arg ...))
(define-syntax-rule (LETREC* Arg ...) (_GLET letrec* Arg ...))

;;  --------------------    Lambda expressions with fewer parentheses.

;;  To enter a Unicode character in a text editor in Debian 9,
;;  press Ctrl+Shift+U <code> space/enter.
;;
;;  <code>  character   description
;;  3bb     λ           lambda
;;
;;  We do not use Unicode Arrows because they are indistinguishable in VSCode 14px font.
;;
;;  2192    →           rightward arrow
;;  21d2    ⇒           rightward double arrow
;;
;;  Examples:
;;  All of these mean the same thing:
;;
;;      (lambda (a b c) (+ a b c))
;;
;;      λ a b c -> (+ a b c)
;;
;;      λ a b c => + a b c

(define-syntax-parser λ
    #:datum-literals (-> =>)
    [(_ Param ... -> ~! Body1 Body ...)
        #'(lambda (Param ...) Body1 Body ...)]
    [(_ Param ... => ~! Body1 Body ...)
        #'(lambda (Param ...) (Body1 Body ...))]
)

(define (!= a b)                (not (= a b)))
(define (not-equal? a b)        (not (equal? a b)))
