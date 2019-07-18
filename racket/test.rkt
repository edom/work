#lang stc-racket

(require
    rackunit
    rackunit/text-ui
)

(define tests
    (test-suite "Suite"
        (check-equal? 1 2 "suite test 1")
        (test-case "Case"
            (check-equal? 2 3 "suite case 1 test 1")
        )
    )
)

(run-tests tests)
