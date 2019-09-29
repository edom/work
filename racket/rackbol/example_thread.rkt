#lang racket

(define (add1 x) (+ x 1))
(define (sleep_add1 x) (sleep 1) (+ x 1))

(define counter 0)

;;  This produces an expected result (counter = 2).
(set! counter 0)
(for-each thread-wait (list
    (thread (lambda () (set! counter (begin (sleep 1) (+ counter 1)))))
    (thread (lambda () (set! counter (begin (sleep 1) (+ counter 1)))))))
counter

;;  This produces an expected result (counter = 2).
(set! counter 0)
(for-each thread-wait (list
    (thread (lambda () (set! counter (begin (sleep 1) (add1 counter)))))
    (thread (lambda () (set! counter (begin (sleep 1) (add1 counter)))))))
counter

;;  This produces an unexpected result (counter = 1).
(set! counter 0)
(for-each thread-wait (list
    (thread (lambda () (set! counter (sleep_add1 counter))))
    (thread (lambda () (set! counter (sleep_add1 counter))))))
counter

;;  This produces an unexpected result (counter = 1),
;;  but not too surprising, because this is merely inlined sleep_add1.
(set! counter 0)
(for-each thread-wait (list
    (thread (lambda () (set! counter [(lambda (x) (sleep 1) (+ x 1)) counter])))
    (thread (lambda () (set! counter [(lambda (x) (sleep 1) (+ x 1)) counter])))))
counter

;;  This produces an unexpected result (counter = 1),
;;  but not too surprising, because I have expected the equivalence:
;;      (let ((Param Arg)) Body) = ((lambda (Param) Body) Arg)
(set! counter 0)
(for-each thread-wait (list
    (thread (lambda () (set! counter (let [(x (+ counter 1))] (sleep 1) x))))
    (thread (lambda () (set! counter (let [(x (+ counter 1))] (sleep 1) x))))))
counter

(set! counter 0)
(define num_thread 10)
(define num_iter 100)
(define threads
    (for/list [(t (in-range num_thread))]
        (thread (lambda ()
            (for [(i (in-range num_iter))]
                (sleep (/ (random 100) 1000))
                (set! counter (begin
                    (print counter)
                    (newline)
                    (+ counter 1))))))))
(set! counter (begin
    (printf "begin long sleep~n")
    (sleep 3)
    (printf "end long sleep~n")
    counter))
(for-each thread-wait threads)
(printf "~a~n" counter)
(printf "~a~n"
    (if (= counter (* num_thread num_iter))
        "Expected."
        "Unexpected."
    ))
