#lang racket

(require (only-in racket (error racket:error)))
(require (prefix-in emacs: "read.rkt"))
(provide eval)
(provide load)

;;  --------------------    Buffers.

;;  Buffer-local variables.
(struct Buffer (vars))
(define current-buffer (Buffer (make-hash)))

;;  --------------------    Namespaces.

(define *values* (make-hash))
(define *functions* (make-hash))

;;  --------------------    Primitives.

(struct Nil ())
(define nil (Nil))

(define (symbol-value name) (hash-ref *values* name))

;;  Must never be passed void values.
(define (symbol-value-set! name value)
    (hash-set! *values* name value))

(define (symbol-value/void name) (hash-ref *values* name void))

;;  Can be passed void values.
(define (symbol-value-set!/void name value)
    (if (void? value)
        (hash-remove! *values* name)
        (hash-set! *values* name value)))

(define (symbol-function name) (hash-ref *functions* name))

(define (symbol-function-set! name value) (hash-set! *functions* name value))

(define (funcall Func . Args) (emacs-apply Func Args))

;;  --------------------    eval

(define (eval Form)
    (match Form
        ;;  --------------------    Self-evaluating forms.
        ((? number?) Form)
        ((cons 'lambda _) Form)
        ;;  --------------------    Other forms.
        ((? symbol?) (symbol-value Form))
        ((list '#%comment Text) nil) ;; TODO buffer-local variables
        ((list 'quote Value) Value)
        ((list 'setq Name Exp) (symbol-value-set! Name (eval Exp)))
        ((cons 'progn Body) (eval-progn Body))
        ;;  --------------------    Applications.
        ((cons Func Args) (emacs-apply (eval-function Func) (map/seq eval Args)))
        (_ ($error "invalid form: ~s" Form))))

;;  The documentation does not guarantee that "map" is sequential.
(define (map/seq f list) (for/list ((x (in-list list))) (f x)))

(define ($error format . args)
    (apply racket:error 'emacs:eval format args))

;;  Func is an Emacs lambda (Racket list whose car is 'lambda), or a Racket lambda.
;;  Args have been evaluated.
(define (emacs-apply Func Args)
    (match Func
        ((? procedure?) (apply Func Args))
        ((list-rest 'lambda Params Body)
            (unless (= (length Params) (length Args))
                (error "arity mismatch: expecting ~a, given ~a" (length Params) (length Args)))
            (define old-values (map/seq symbol-value/void Params))
            (define pre-thunk-run? #f)
            ;;  I'm not sure whether this dynamic-wind is correct.
            (dynamic-wind
                (lambda ()
                    (when pre-thunk-run? (error "trying to reenter emacs-apply by continuation"))
                    (set! pre-thunk-run? #t)
                    (for ((param (in-list Params)) (arg (in-list Args)))
                        (symbol-value-set! param arg)))
                (lambda ()
                    (eval (cons 'progn Body)))
                (lambda ()
                    (for ((param (in-list Params)) (old (in-list old-values)))
                        (symbol-value-set!/void param old)))))
        (else ($error "cannot apply: ~s" Func))))

;;  Normalize the term at the function position in an application expression.
(define (eval-function Form)
    (define (loop Cur) ;; to blame the original Form in case of errors
        (match Cur
            ((? symbol?) (loop (symbol-function Cur)))
            ((? procedure?) Cur)
            ((cons 'lambda _) Cur)
            (else ($error "form does not evaluate to lambda at function position: ~s" Form))))
    (loop Form))

(define (eval-progn Forms)
    (match Forms
        ((list) nil)
        ((list Exp) (eval Exp))
        ((list-rest Exp Rest) (begin (eval Exp) (eval-progn Rest)))))

;;  --------------------    load

(define (load path)
    (emacs:with-input-from-file path (lambda ()
        (for ((expr (in-port emacs:read)))
            (eval expr)))))

;;  --------------------    Initialization.

(define (my-print value)
    (printf "~n~v~n" value))

(define builtin-functions (hash
    'eval eval
    'print my-print
    '+ +
    'funcall funcall
    'fset symbol-function-set!
))
(set! *functions* (hash-copy builtin-functions))
