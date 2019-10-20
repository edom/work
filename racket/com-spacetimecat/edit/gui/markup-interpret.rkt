#lang s-exp "lang.rkt"

(require
    (only-in "markup.rkt" markup?)
    "box.rkt"
    "font-list.rkt"
)

(provide
    interpret/measure
    interpret/render
)

(define current-font-list               (make-parameter (new my-font-list%)))
(define (interpret/measure dc program)  (internal-interpret dc program #:dry-run? #t))
(define (interpret/render dc program)   (internal-interpret dc program #:dry-run? #f))

;;  The key idea/design-principle of the markup language is
;;  that Display-Context (DC) modifications should be scoped.
;;
;;  The evaluation of a program produces a box.
;;
;;  A box may nest inside another box.
;;
;;  Program ::= empty
;;          |   (string String)
;;          |   (bold Program)
;;          |   (hflow (Program ...))   ;;  horizontal (left-to-right) juxtaposition of boxes
;;          |   (vflow (Program ...))   ;;  vertical (top-to-bottom) juxtaposition of boxes

(define/contract (internal-interpret dc program #:dry-run? (dry-run? #f))
    (-> (is-a?/c dc<%>)
        markup?
        #:dry-run? boolean?
        Box?)
    (define font-list (current-font-list))
    (define x 0)
    (define y 0)
    (define/contract (loop program)
        (-> markup? Box?)
        (match program
            [`(empty)           (Box x y 0 0)]
            [`(string ,str)     (i-string str)]
            [`(bold ,prog)      (i-bold prog)]
            [`(hflow ,progs)    (i-hflow progs)]
            [`(vflow ,progs)    (i-vflow progs)]
        ))
    (define (i-string str)
        (define-values (w h _0 _1) (send dc get-text-extent str))
        (unless dry-run? (send dc draw-text str x y))
        (Box x y w h))
    (define (i-hflow progs)
        (define prev-x x)
        (define box (list-foldl union-Box
                                (Box x y 0 0)
                                (list-map (λ p -> (define b (loop p))
                                                    (set! x (Box-x1 b))
                                                    b)
                                            progs)))
        (set! x prev-x)
        box)
    (define (i-vflow progs)
        (define prev-y y)
        (define box (list-foldl union-Box
                                (Box x y 0 0)
                                (list-map (λ p -> (define b (loop p))
                                                    (set! y (Box-y1 b))
                                                    b)
                                            progs)))
        (set! y prev-y)
        box)
    (define (i-bold prog)
        (with-preserve-font dc
            (send dc set-font (send font-list bold-of (send dc get-font)))
            (loop prog)))
    (loop program))

(define (call-preserve-font dc body)
    (define font #f)
    (dynamic-wind
        (λ -> (set! font (send dc get-font)))
        body
        (λ -> (send dc set-font font))))

(define-syntax-rule (with-preserve-font dc body ...)
    (call-preserve-font dc (λ -> body ...)))
