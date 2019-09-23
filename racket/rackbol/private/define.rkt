#lang rackbol/private/lang

(provide (all-defined-out))
(require (for-syntax syntax/strip-context))
(require "log.rkt")

;;  https://stackoverflow.com/questions/38130826/can-i-instantiate-a-module-multiple-times-in-one-racket-program

(define DEFINITIONS (make-hash))

;;  The "DEFINE" form.

(begin-for-syntax
    (define-syntax-class type%
        #:datum-literals (STORAGE TABLE)
        (pattern STORAGE)
        (pattern TABLE)
    )
    (define-splicing-syntax-class procedure_input%
        #:datum-literals (DEFAULT INPUT NAME TYPE)
        (pattern (INPUT Id
            (~optional (~seq NAME Name) #:defaults ((Name #''Id)))
            TYPE Type
            (~optional (~seq DEFAULT Default) #:defaults ((Default #f)))
        ))
    )
    (define-splicing-syntax-class procedure_output%
        #:datum-literals (NAME OUTPUT TYPE)
        (pattern (OUTPUT Id
            (~optional (~seq NAME Name) #:defaults ((Name #''Id)))
            TYPE Type
        ))
    )
)

(define (read_input name)
    (printf "Input ~a: " name)
    (read))

(define-syntax-parser make_alist
    [(_ (~seq Key Val) ...)
        #'(list (cons `Key Val) ...)])

(define-syntax-parser DEFINE
    #:datum-literals (ACTION PROCEDURE VALUE)
    [(_ Type:type% Name (~seq Key Val) ...)
        #'(begin
            (define Name `((Key . Val) ...))
            (hash-set! DEFINITIONS 'Name Name)
        )
    ]
    [(_ value Name Value)
        #'(begin
            (define Name Value)
            (hash-set! DEFINITIONS 'Name Name)
        )
    ]
    [(_ PROCEDURE Name
            (~alt
                Input:procedure_input%
                Output:procedure_output%
                (~once (ACTION Action ...))
            )
            ...
        )
        ;;  TODO:
        ;;  Decouple model-building and code-generation.
        ;;  Put model-building here.
        ;;  Put code-generation in generate.rkt.
        #'(begin
            (define (Name)
                (define version 0)
                (define execution_id (random 1048576))
                (define t0 (current-milliseconds))
                (write_log (list version execution_id 'Name 'start))
                (define Input.Id (read_input Input.Name)) ...
                (define Output.Id #f) ...
                Action ...
                (printf "~a = ~v~n" Output.Name Output.Id) ...
                (define t1 (current-milliseconds))
                (write_log (list version execution_id 'Name 'stop (- t1 t0)))
            )
            (hash-set! DEFINITIONS 'Name Name)
            #|
            ;;  Should we define struct Procedure with a prop:procedure property?
            (hash-set! DEFINITIONS 'Name `(
                (type . procedure)
                (name . Name)
                (inputs . ((Input.Id Input.Type) ...))
                (outputs . (Output.Id ...))
                (action . #'(Action ...))
                (code . ,Code)
            ))
            |#
        )
    ]
)
