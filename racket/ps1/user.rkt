#lang racket

(require "machine.rkt")

(provide
    in_bios_rom_pc_values
    decode_instruction_at
    trace
    notrace
    break_at!
    init_session!
    load_state
    save_state
    delete_state
    default_state_snapshot_path
)
(provide (all-from-out "machine.rkt"))

;;  --------------------    Load-save state.

(GENERATE FUNCTION FOR STATE SNAPSHOT
    make_state_snapshot
    restore_state_snapshot!
)

(require racket/fasl)

(define (default_state_snapshot_path) "_state_snapshot")

(define (save_state #:to [path (default_state_snapshot_path)])
    (call-with-output-file path #:mode 'binary #:exists 'truncate (lambda (port)
        (s-exp->fasl (make_state_snapshot) port #:keep-mutable? #t))))

(define (load_state #:from [path (default_state_snapshot_path)])
    (call-with-input-file path #:mode 'binary (lambda (port)
        (restore_state_snapshot! (fasl->s-exp port)))))

(define (delete_state #:at [path (default_state_snapshot_path)])
    (delete-file path))

(define (init_load_state!)
    (define (on_load_fail e)
        (printf "INFO: Could not load default state snapshot file: ~a~n" (default_state_snapshot_path))
    )
    (with-handlers [(exn:fail:filesystem? on_load_fail)]
        (load_state #:from (default_state_snapshot_path))
        (printf "Loaded state from ~a~n" (default_state_snapshot_path))))

;;  --------------------    User.

(define (in_bios_rom_pc_values)
    (in-range BIOS_ROM_MIN_ADDRESS BIOS_ROM_MAX_ADDRESS_1 4))

(define (trace) (set_trace_enabled! #t))
(define (notrace) (set_trace_enabled! #f))

(define (break_at! address) (set_breakpoint_at! address))

(define (init_session!)
    (init_load_state!)
    ;(trace)
    ;(break_at! #xBFC003B4)
    (for ([pc (in_bios_rom_pc_values)])
        (define dec (decode_instruction_at pc))
        (match dec
            [`(set (c ,_ ,_ ,_) ,_)
                (break_at! (bitwise-ior pc #xA0000000))
            ]
            [_ (void)]
        )
    )

)
