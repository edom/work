#lang racket/base

(require
    (for-syntax
        racket/base
    )
    syntax/parse
    syntax/parse/define
    racket/match
    "buffer.rkt"
)
(provide
    assemble!
)

;;  --------------------    Assembly.
;;
;;  Is open-output-bytes + get-output-bytes + write-byte faster?

(define-syntax-parser BYTE
    #:datum-literals (OF)
    [   (_ index OF integer)
        #'(bitwise-bit-field integer (* 8 index) (* 8 (+ index 1)))
    ])

(define (assemble! out ins-list)
    (define-syntax-rule (w Byte ...)
        (begin
            (Buffer-write-byte! out Byte)
            ...
        ))
    (define (imm64 integer)
        (for ((index [in-range 8]))
            (w (BYTE index OF integer))
        ))
    (for ([ins (in-list ins-list)])
        (match ins
            ;; REX.W B8+ rd io
            [`(mov rax ,i) (w #x48 #xB8) (imm64 i)]
            [`(ret) (w #xC3)]
            [_ (error 'assemble! "unknown instruction ~a" ins)]
        )))

