#lang s-exp "lang.rkt"

(provide
    (struct-out Box)
    Box-x1
    Box-y1
    union-Box
)

;;  A Box is an axis-aligned rectangle.
(struct Box (x y w h) #:prefab)

(define (Box-x1 b) (+ (Box-x b) (Box-w b)))
(define (Box-y1 b) (+ (Box-y b) (Box-h b)))

;;  Compute the smallest Box that contains the given Boxes.
(define (union-Box a b)
    (match-let* (
            ;;  top-left coordinates, dimensions
            [(Box xa ya wa ha)  a]
            [(Box xb yb wb hb)  b]
            [a-empty?           (or (<= wa 0) (<= ha 0))]
            [x1                 (max (+ xa wa) (+ xb wb))]
            [y1                 (max (+ ya ha) (+ yb hb))]
            [x                  (min xa xb)]
            [y                  (min ya yb)]
            [w                  (- x1 x)]
            [h                  (- y1 y)]
            [union              (Box x y w h)]
        )
        (if a-empty? b union)))