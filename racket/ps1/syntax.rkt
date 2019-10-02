#lang racket

(provide (all-defined-out))

(require racket/stxparam)
(require syntax/parse/define)

;;  --------------------    Infrastructure.

(define TRACE_ENABLED #f)

(define (set_trace_enabled! flag)
    (set! TRACE_ENABLED flag))

(define (warn . msg)
    (printf "WARNING: ~a~n" (apply string-append msg)))

(define (!= a b) [not (= a b)])

;;  No "0x" prefix.

(define (hex4 num)
    (~r num #:base 16 #:min-width 8 #:pad-string "0"))

(define-syntax-rule (LOCAL Arg ...)
    ([lambda () Arg ... ]))

(define-syntax-rule (FOREVER Body ...)
    (letrec (
            [loop (lambda ()
                Body ...
                (loop)
            )])
        (loop)))

(define-syntax-rule (WHILE Cond Body ...)
    (letrec (
            [loop (lambda ()
                (when Cond
                    Body ...
                    (loop))
            )])
        (loop)))

;;  --------------------    Ports.

(struct Port [id name address size (value #:mutable) on_write] #:prefab)

(define PORTS_TABLE (make-hash))
(define (make_port_key address size) (cons address size))
(define (get_port_key port) (make_port_key [Port-address port] [Port-size port]))

(define (get_port address size)
    (define port (hash-ref PORTS_TABLE (make_port_key address size) #f))
    (unless port (error 'get_port "There is no ~a-byte port at address ~a" size [hex4 address]))
    port)

(define (read_port #:address address #:size size)
    (define port (get_port address size))
    (when TRACE_ENABLED
        (printf "TRACE: read_port ~a ~a (~a)~n" size (hex4 address) (Port-name port)))
    (Port-value port))

(define (write_port #:address address #:size size #:value value)
    (define port (get_port address size))
    (when TRACE_ENABLED
        (printf "TRACE: write_port ~a ~a ~a (~a)~n" size (hex4 address) (hex4 value) (Port-name port)))
    ([Port-on_write port] port (Port-value port) value)
    (set-Port-value! port value))

(define-syntax-parameter $old_value [syntax-rules ()])
(define-syntax-parameter $new_value [syntax-rules ()])

(begin-for-syntax
    (define-splicing-syntax-class Port_On_Write
        #:datum-literals (DO EXPECT ON VALUE WRITE)
        [pattern (~seq ON WRITE DO Expr:expr ...)
            #:with Lambda #'(lambda (_port _old_value _new_value)
                (syntax-parameterize (
                        [$old_value (make-rename-transformer #'_old_value)]
                        [$new_value (make-rename-transformer #'_new_value)]
                    )
                    Expr ...
                ))]
        [pattern (~seq ON WRITE EXPECT VALUE Value:number)
            #:with Lambda #'(lambda (_port _old_value _new_value)
                (unless (= _new_value Value)
                    (error 'write_port "Expecting ~a" Value)
                ))]
        [pattern (~seq)
            #:with Lambda #'(lambda (_port _old_value _new_value) (void))]
    )
)

(begin-for-syntax
    ;;  List of identifier syntax objects.
    (define STATE_LIST '())
    (define PORTS_LIST '())
    (define (add_state! Id) (set! STATE_LIST (cons Id STATE_LIST)))
    (define (add_port! Id) (set! PORTS_LIST (cons Id PORTS_LIST)))
)

(define-syntax-parser DEFINE
    #:datum-literals [AT PORT SIZE]
    ([_ PORT Id:id Name:string
            AT Address:number
            SIZE Size:number
            On_Write:Port_On_Write]
        #'(begin
            (begin-for-syntax (add_port! #'Id))
            (define Id (Port 'Id Name Address Size 0 On_Write.Lambda))
            (hash-set! PORTS_TABLE (get_port_key Id) Id)
        ))
    ([_ STATE Id:id Init:expr]
        #'(begin
            (begin-for-syntax (add_state! #'Id))
            (define Id Init)
        ))
)

;;  Limitations for load-save-state generation:
;;
;;      -   A mutable structure must not be nested inside a mutable structure.
;;      -   A cons must not contain a mutable structure.
;;
;;  This is a workaround because Racket's "read" only returns immutable vectors, hash-tables, etc.

(define (perhaps_make_mutable_copy thing)
    (cond
        ((hash? thing) (hash-copy thing))
        ((vector? thing) (vector-copy thing))
        (else thing)))

(define (before_write_state thing) thing)
(define (after_read_state thing) (perhaps_make_mutable_copy thing))

(define (read_state_elem table key) (after_read_state (hash-ref table key)))
(define (make_state_cons key value) (cons key (before_write_state value)))

(define-syntax-parser GENERATE
    #:datum-literals [FUNCTION LOAD READ SAVE STATE WRITE]
    ([_ READ WRITE STATE FUNCTION Read:id Write:id]
        (with-syntax (
                [(State ...) STATE_LIST]
                [(Port ...) PORTS_LIST]
            )
            #'(begin
                (define (Read table)
                    (set! State (read_state_elem table 'State)) ...
                    (set-Port-value! Port (read_state_elem table 'Port)) ...
                )
                (define (Write)
                    (make-hash (list
                        (make_state_cons 'State State) ...
                        (make_state_cons 'Port [Port-value Port]) ...
                    ))
                ))))
)

