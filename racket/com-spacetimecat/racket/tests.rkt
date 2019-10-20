#lang s-exp "lang.rkt"

(require
    rackunit
    rackunit/text-ui
    (only-in racket/control
        call/prompt
        abort/cc
        call/comp
    )
    "fewer-parens.rkt"
)

(provide main)

;;  Short alias     Long name
;;
;;  call/prompt     call-with-continuation-prompt
;;  abort/cc        abort-current-continuation
;;
;;  call/cc         call-with-current-continuation
;;  call/ec         call-with-escape-continuation
;;  call/comp       call-with-composable-continuation

(define-test-suite continuations
    (test-case "Jumping out of lambda with call/ec"
        (define wrong #f)
        (call/ec (λ return ->
            ;;  Note: (return) produces zero values.
            ;;  It is not the same as (void) which produces one value.
            ;;  However, (return (void)) produces one value.
            (return) ;; C users can think of this as a return statement.
            (set! wrong #t)))
        (check-false wrong))
    (test-case "Basic continuation returning a value"
        (define wrong #f)
        (define value (call/cc (λ return ->
            (return 0)
            (set! wrong #t))))
        (check-false wrong)
        (check-equal? value 0))
    (test-case "Looping with call/cc"
        (define counter 0)
        (define repeat
            ;;  (repeat Val) jumps here and returns Val to define,
            ;;  and thus sets repeat to Val.
            ;;  (repeat) would set repeat to void and would cause an error on the second iteration.
            (call/cc (λ x -> x)))
        (set! counter (+ counter 1))
        (when (< counter 5) (repeat repeat))
        (check-equal? counter 5))
    (test-case "Backward GOTOs with call/cc"
        (define counter 0)
        (define repeat #f)
        ;;  C users can think of this as a goto label.
        (call/cc (λ c -> (set! repeat c) c))
        ;;  (repeat) jumps here.
        (set! counter (+ counter 1))
        (when (< counter 5) (repeat))
        (check-equal? counter 5))
    (test-case "The continuation produced by call/ec can only be called from inside the lambda."
        (define counter 0)
        (define escape #f)
        (call/ec (λ c -> (set! escape c) c))
        (check-exn exn? (λ -> (escape))))
    (test-case "Prompts delimit continuations"
        (define result '())
        (define (prepend! x) (set! result (cons x result)))
        (define tag-inner (make-continuation-prompt-tag))
        (define tag-outer (make-continuation-prompt-tag))
        (call/prompt
            (λ -> (call/prompt
                    (λ ->   (abort/cc tag-outer)
                            (prepend! 0))
                    tag-inner
                    (λ ->   (prepend! 1)
                            (abort/cc tag-outer)))
                (prepend! 2))
            tag-outer
            (λ -> (prepend! 3)))
        ;;  (abort/cc tag-outer) can't jump out any farther than here.
        (check-equal? result (list 3)))
    (test-case "Can jump from inside a barrier to its outside"
        (call/ec (λ return ->
            (call-with-continuation-barrier (λ ->
                (return)))))
        (void))
    (test-case "Cannot jump from outside a barrier to its inside"
        (define cont #f)
        (define counter 0)
        (call-with-continuation-barrier (λ ->
            (call/cc (λ c -> (set! cont c)))
            ;;  Calling cont jumps here.
            (set! counter (+ counter 1))
            (when (< counter 2) (cont))
            (check-equal? counter 2)))
        (check-exn exn? cont))
)

(define (main)
    (run-tests continuations)
    (void))
