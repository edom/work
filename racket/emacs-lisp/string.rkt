#lang racket

;   --------------------    Emacs string.

;;  An Emacs char literal such as ?A evaluates to an integer such as 65.
;;  An Emacs character is an integer; Emacs do not have a dedicated char type.
;;  An Emacs string is an array of Emacs characters.
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Modifying-Strings.html
;;  Thus Racket fxvector is more suitable than bytes to represent Emacs string.
;;  But, a complication:
;;  "The function [aset] converts a unibyte string to multibyte if necessary to insert a character."
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Array-Functions.html

(require racket/fixnum)
(provide (all-defined-out))

;;  Bit 27 of character (integer) is the Meta bit.
(define meta-mask (arithmetic-shift 1 27))

(struct String (array)
    #:mutable
    #:methods gen:custom-write (
        (define (write-proc object port mode)
            (define repr (String->string/unfaithful object))
            (case mode
                ((#t) (write repr))
                ((#f) (display repr))
                (else (print repr port mode))))))

;;  Unclear semantics.
;;  For troubleshooting syntax trees.
;;  Not for serialization/round-tripping.
(define (String->string/unfaithful object)
    (define fxs (String-array object))
    ;;  This may create a lot of garbage.
    (apply string-append
        (for/list ((c (in-fxvector fxs)))
            (define my-meta-mask (bitwise-and c meta-mask))
            (define my-char-code (bitwise-and c (- meta-mask 1)))
            (string-append
                (if (= my-meta-mask 0) "" "\\M-")
                (string (integer->char my-char-code))))))

(define (make-String n)
    (String (make-fxvector n)))

(define (String-set! str idx val)
    (fxvector-set! (String-array str) idx val))

(define (String-length str)
    (fxvector-length (String-array str)))

(define (String-copy! dst dst-start src (src-start 0) (src-end (String-length src)))
    (fxvector-copy! (String-array dst) dst-start (String-array src) src-start src-end))

(define (fxvector-copy! dst dst-start src src-start src-end)
    (define count (- src-end src-start))
    (for ((i (in-range 0 count)))
        (fxvector-set! dst (+ dst-start i) (fxvector-ref src (+ src-start i)))))

(define (String-substring str beg end)
    (define sub (make-String (- end beg)))
    (String-copy! sub 0 str beg end)
    sub)
