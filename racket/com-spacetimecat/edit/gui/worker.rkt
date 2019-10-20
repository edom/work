#lang s-exp "lang.rkt"

(provide
    worker<%>
    worker%
    make-worker
)

(define input? any/c)
(define output? any/c)

(define worker<%> (interface ()
    (set-input (->m input? any/c))
    (before-compute (->m input? any/c))
    (after-compute (->m input? output? any/c))
))

(define worker% (class* object% (worker<%>) (super-new)
    (define current-input (box #f))
    (define worker-thread (make-thread))

    ;;  Begin computation on worker thread if it is idle.
    ;;  Otherwise replace the scheduled computation.
    ;;  2019-10-20: Design Question: Should set-input kill the worker thread?
    (define/public (set-input input)
        (set-box! current-input input)
        (before-compute input)
        (thread-send (get-thread/ensure) input))

    ;;  Subclasses should override this method with the potentially long-running operation.
    (define/public (compute input) input)

    ;;  Subclasses may override these.
    ;;  Both of these run on the UI thread.
    (define/public (before-compute input) (void))
    (define/public (after-compute input answer) (void))

    (define/private (make-thread)
        (thread (λ ->
            (define (loop)
                (define input (thread-receive))
                (when (eq? (unbox current-input) input)
                    (define output (compute input))
                    (when (box-cas! current-input input #f)
                        (queue-callback (λ -> (after-compute input output)))))
                (loop))
            (loop))))

    (define/private (get-thread/ensure)
        (unless (thread-running? worker-thread)
            ;;  The worker thread may have died from an unhandled exception.
            (set! worker-thread (make-thread)))
        worker-thread)
))

;;  before-compute runs on the UI thread.
;;  after-compute runs on the UI thread.
;;  compute runs on a separate background thread.
;;  Note that Racket's multithreading is cooperative,
;;  so compute should yield periodically,
;;  or other threads may not get a chance to run.
(define/contract (make-worker
        #:compute [i-compute (λ input -> input)]
        #:before-compute [i-before-compute (λ input -> (void))]
        #:after-compute [i-after-compute (λ input output -> (void))])
    (->* ()
         (#:compute (-> input? output?)
          #:before-compute (-> input? any/c)
          #:after-compute (-> input? output? any/c))
         (is-a?/c worker<%>))
    (new (class worker% (super-new)
        (define/override (compute input) (i-compute input))
        (define/override (before-compute input) (i-before-compute input))
        (define/override (after-compute input output) (i-after-compute input output))
    )))
