#lang racket

(require (for-syntax racket/syntax))
(require racket/serialize)
(require racket/stxparam)
(require syntax/parse/define)

(provide
    != LOCAL FOREVER WHILE THREAD
    bytes_read_unsigned bytes_write_unsigned
    TRACE_ENABLED set_trace_enabled!
    warn
    hex1 hex2 hex4
    read_port write_port
    DEFINE $old_value $new_value
    GENERATE
)

;;  --------------------    Racket.

(define (!= a b) [not (= a b)])

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

(define-syntax-rule (THREAD Body ...)
    (thread (lambda () Body ...)))

(define (bytes_read_unsigned #:from bytes #:at offset #:size size)
    (define signed? #f)
    (define big-endian? #f)
    (integer-bytes->integer bytes signed? big-endian? offset (+ offset size)))

(define (bytes_write_unsigned value #:to bytes #:at offset #:size size)
    (define signed? #f)
    (define big-endian? #f)
    (integer->integer-bytes value size signed? big-endian? bytes offset)
    (void))

(define (hash-copy! dst src)
    (hash-clear! dst)
    (hash-for-each src (lambda (k v) (hash-set! dst k v))))

;;  --------------------    Infrastructure.

(define TRACE_ENABLED #f)

(define (set_trace_enabled! flag)
    (set! TRACE_ENABLED flag))

(define (warn . msg)
    (printf "WARNING: ~a~n" (apply string-append msg)))

;;  No "0x" prefix.

(define (hex1 num) (~r num #:base 16 #:min-width 2 #:pad-string "0"))
(define (hex2 num) (~r num #:base 16 #:min-width 4 #:pad-string "0"))
(define (hex4 num) (~r num #:base 16 #:min-width 8 #:pad-string "0"))

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
    (define-splicing-syntax-class State_After
        #:datum-literals (AFTER DO RESTORE)
        [pattern (~seq AFTER RESTORE DO Expr ...) #:with (Body ...) #'(Expr ...)]
        [pattern (~seq) #:with (Body ...) #'()]
    )
)

(begin-for-syntax
    (struct State1 (id restore_id) #:prefab)
    ;;  List of identifier syntax objects.
    (define STATES '())
    (define PORTS_LIST '())
    (define (add_state! x) (set! STATES (cons x STATES)))
    (define (add_port! Id) (set! PORTS_LIST (cons Id PORTS_LIST)))
)

(define-syntax-parser DEFINE

    #:datum-literals [AT PORT SIZE STATE]

    ([_ PORT Id:id Name:string
            AT Address:number
            SIZE Size:number
            On_Write:Port_On_Write]
        #'(begin
            (define Id (Port 'Id Name Address Size 0 On_Write.Lambda))
            (hash-set! PORTS_TABLE (get_port_key Id) Id)
            (begin-for-syntax (add_port! #'Id))
        ))

    ;;  The programmer is responsible for avoiding pointing a top-level variable to a part of an object.
    ;;  State-restoration is not always a simple set! because the operation is only correct if the object graph is preserved.
    ;;  For example, without AFTER RESTORE DO (initialize_region!), things like RAM_Region_0 in "machine.rkt"
    ;;  would unexpectedly point to the previous object.

    ([_ STATE Id:id Init:expr
            After_Restore:State_After
        ]
        (with-syntax* (
                [Restore_Id (format-id #'Id "_restore_~a!" (syntax-e #'Id) #:source #'Id)]
            )
            #'(begin
                (define Id Init)
                (define (Restore_Id x)
                    (set! Id x)
                    After_Restore.Body ...
                )
                (begin-for-syntax (add_state! (State1 #'Id #'Restore_Id)))
            )))
)

;;  Limitation for load-save-state generation:
;;  Top-level variables must not refer to a part of an object.
;;  Think in terms of whether the object graph can be replaced completely.

(define (before_write_state thing) thing)
(define (after_read_state thing) thing)

(define (read_state_elem table key) (after_read_state (hash-ref table key)))
(define (make_state_cons key value) (cons key (before_write_state value)))

(define-syntax-parser GENERATE
    #:datum-literals [FOR FUNCTION SNAPSHOT STATE]
    ([_ FUNCTION FOR STATE SNAPSHOT Make:id Restore:id]
        (with-syntax (
                [(#s(State1 State_Id State_Restore_Id) ...) STATES]
                [(Port ...) PORTS_LIST]
            )
            #'(begin
                (define (Make)
                    (serialize
                        (make-hash (list
                            (make_state_cons 'State_Id State_Id) ...
                            (make_state_cons 'Port [Port-value Port]) ...
                        ))))
                (define (Restore ser)
                    (define table (deserialize ser))
                    (State_Restore_Id (read_state_elem table 'State_Id)) ...
                    (set-Port-value! Port (read_state_elem table 'Port)) ...
                ))))
)
