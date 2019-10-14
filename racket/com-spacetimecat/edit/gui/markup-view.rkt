#lang s-exp "lang.rkt"

(require
    racket/match
    (prefix-in m: "markup.rkt")
    (only-in "markup.rkt"
        markup/c
    )
)

(provide
    markup-canvas%
)

;;  The key idea/design-principle of the markup language is
;;  that Display-Context (DC) modifications should be scoped.
;;
;;  The evaluation of a program produces a box.
;;
;;  A box may nest inside another box.
;;
;;  Program ::= empty
;;          |   (string String)
;;          |   (hflow (Program ...))   ;;  horizontal (left-to-right) juxtaposition of boxes
;;          |   (bold Program)

(struct Box (x y w h) #:prefab)

(define (Box-x1 b) (+ (Box-x b) (Box-w b)))
(define (Box-y1 b) (+ (Box-y b) (Box-h b)))

(define (add-Box a b)
    (Let*   ;;  top-left coordinates, dimensions
            xa (Box-x a) ya (Box-y a) wa (Box-w a) ha (Box-h a)
            xb (Box-x b) yb (Box-y b) wb (Box-w b) hb (Box-h b)
            ;;  bottom-right coordinates
            x1 (max (+ xa wa) (+ xb wb))
            y1 (max (+ ya ha) (+ yb hb))
            ;;  result
            x (min xa xb)
            y (min ya yb)
            w (- x1 x)
            h (- y1 y)
    #:in    (if (or (<= wa 0) (<= ha 0))
                b
                (Box x y w h)
            )))

(define my-font-list% (class font-list% (super-new)
    (define/public (bold-of font)
        (send this find-or-create-font
            (send font get-size)
            (send font get-family)
            (send font get-style)
            'bold
            (send font get-underlined)
            (send font get-smoothing)
            (send font get-size-in-pixels)
            (send font get-hinting)
        ))
))

(define markup-canvas% (class canvas%

    (init-field [markup (m:empty)])

    ;;  Do not use 'transparent.
    ;;  The text somehow gets drawn twice and overlaps.

    (super-new [style '(no-focus)])

    (inherit get-dc min-width min-height)

    (define font-list (new my-font-list%))

    (define normal-font normal-control-font)
    (define bold-font (send font-list bold-of normal-font))

    (define (call-preserve-font dc body)
        (define font #f)
        (dynamic-wind
            (λ -> (set! font (send dc get-font)))
            body
            (λ -> (send dc set-font font))
        ))

    (define-syntax-rule (with-preserve-font dc body ...)
        (call-preserve-font dc (λ -> body ...)))

    ;;  Program satisfies markup/c.
    ;;
    ;;  Return a Box.

    (define (interpret program #:dry-run? (dry-run? #f))
        (define dc (get-dc))
        (define/contract (loop x y program)
            (-> real? real? markup/c Box?)
            (match program
                [`(string ,str)
                    (define-values (w h _0 _1) (send dc get-text-extent str))
                    (unless dry-run? (send dc draw-text str x y))
                    (Box x y w h)
                ]
                [`(hflow ,progs)
                    (list-foldl add-Box
                        (Box x y 0 0)
                        (list-map
                            (λ p ->
                                (define b (loop x y p))
                                (set! x (Box-x1 b))
                                b
                            )
                            progs
                        )
                    )
                ]
                [`(bold ,prog)
                    (with-preserve-font dc
                        (send dc set-font (send font-list bold-of (send dc get-font)))
                        (loop x y prog)
                    )
                ]
            )
        )
        (loop 0 0 program)
    )

    (define (recompute-min-size)
        (define box (interpret markup #:dry-run? #t))
        (define (ceil x) (inexact->exact (ceiling x)))
        (min-width (ceil (Box-w box)))
        (min-height (ceil (Box-h box)))
    )

    (define/public (get-markup) markup)

    (define/public (set-markup m)
        (set! markup m)
        (recompute-min-size)
        (send this refresh)
    )

    (recompute-min-size)

    (define/override (on-paint)
        (super on-paint)
        (interpret markup)
    )
))
