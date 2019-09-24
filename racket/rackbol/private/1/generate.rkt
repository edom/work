#lang s-exp "_in.rkt"

(require+provide/all "ontology.rkt")

(provide (all-defined-out))

;;  --------------------    Code-generation.

(define-syntax-parser CALL
    [(_ Proc Arg ...)
        #'([GET lambda OF Proc] Arg ...)
    ]
)

(define (read_input name)
    (printf "Input ~a: " name)
    (read))

(define-syntax-parser GENERATE
    #:datum-literals [FOR LAMBDA PROCEDURE]
    [(_)
        #'(for/list [([id object] [in-hash DEFINITIONS])]
            #`(pretty-print #,object)
            ; (GENERATE LAMBDA FOR PROCEDURE)
        )
    ]
    [(_ LAMBDA FOR PROCEDURE Id)
        #'(error "Not implemented: GENERATE LAMBDA FOR PROCEDURE")
        #|
        (case type
            [(Procedure)
                (SET lambda OF object TO #'[lambda ()
                    (define Name '#,Name)
                    (define version 0)
                    (define execution_id (random 1048576))
                    (define t0 (current-milliseconds))
                    (write_log (list version execution_id Name 'start))
                    (define Input.Id (read_input Input.Name)) ...
                    (define Output.Id #f) ...
                    Action ...
                    (printf "~a = ~v~n" Output.Name Output.Id) ...
                    (define t1 (current-milliseconds))
                    (write_log (list version execution_id Name 'stop (- t1 t0)))
                ])
            ]
        )
        |#
    ]
)

;;  --------------------    Rackbol-specific classes.

(CLASS Machine
    [PROPERTY name]
    [PROPERTY address]
)
(CLASS Storage
    [PROPERTY type]
    [PROPERTY name]
)
(CLASS Procedure
    [PROPERTY name]
    [PROPERTY input]
    [PROPERTY output]
)
