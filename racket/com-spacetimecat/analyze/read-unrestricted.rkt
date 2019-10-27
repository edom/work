#lang s-exp "lang.rkt"

;;  If you can, use read.rkt, and avoid these unrestricted procedures.

(provide
    read-module-from-file
    make-namespace-from-file
)

;;  Security Note: #lang and #reader can execute arbitrary code.

(define (read-module-from-file path)
    (parameterize [ (read-accept-lang #t)
                    (read-accept-reader #t) ]
        (read-syntax-from-file path)))

(define (make-namespace-from-file path)
    (parameterize ([current-namespace (make-base-namespace)])
        (dynamic-require path #f)
        (module->namespace path)))

(require racket/match)

;;  The output should be piped to "less -S".
(define/contract (print-syntax stx)
    (-> syntax? any/c)
    (define (loop indent obj)
        (define indent1 (string-append indent "  "))
        (match obj
            [(? syntax? stx)
                (define info (syntax-debug-info stx))
                (printf "~astx context=~s bindings=~s~n"
                    indent
                    (hash-ref info 'context)
                    (hash-ref info 'bindings '()))
                (loop indent1 (syntax-e stx))]
            [(? list? lst)
                (printf "~alist~n" indent)
                (list-for-each (λ elem -> (loop indent1 elem)) lst)]
            [(cons a b)
                (printf "~acons~n" indent)
                (loop indent1 a)
                (loop indent1 b)]
            [datum (printf "~a~s~n" indent datum)]
        ))
    (loop "" stx))

;;  Test.
(begin
    (require racket/pretty)
    (provide main)
    (define (annotate stx)
        (syntax-case stx ()
            [(module Id Par (#%module-begin Body ...))
                #`(module Id Par (#%module-begin
                    ;;  2019-10-21: Why does this not work after expand?
                    (require "read-unrestricted.rkt")
                    (define FOO 300)
                    (displayln FOO)
                    Body ...))]
        ))
    (define-namespace-anchor -anchor)
    (define (main)
        (define mod-file (build-path/simplify $__FILE__/path ".." "test-data.rkt"))
        (define mod-dir (build-path/simplify mod-file ".."))
        (parameterize ([current-load-relative-directory mod-dir]
                       [current-namespace (namespace-anchor->namespace -anchor)])
            (define mod1 (read-module-from-file mod-file))
            (define mod2 (annotate mod1))
            (define mod3 (namespace-syntax-introduce mod2))
            (define mod4 (expand-syntax mod3))
            ;; (define mod5 (annotate mod4))
            ;; doesn't work: (define mod6 (expand-syntax mod5))
            (define mod-to-print mod4)
            (print-syntax mod-to-print)
            (void)
        )
    ))
