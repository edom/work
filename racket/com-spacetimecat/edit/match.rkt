#lang s-exp "lang.rkt"

(provide
    (struct-out Match)
    make-empty-Match
    Match-fail?
    match-string
)

;;  Important: The "indexes/ascending" field contains an _ascending_ list of indexes.

(struct Match (string succeed? indexes/ascending score) #:prefab)

(define (make-empty-Match) (Match "" #f '() 0))

(define (Match-fail? m) (not (Match-succeed? m)))

;;  The scoring system is similar to that of some arcade games:
;;  There is a bonus multiplier for combos.
;;
;;  s, t are strings: s is user input; t is a choice.
;;  i, j are zero-based positions: i is for s; j is for t.
;;  m, n are exclusive upper bounds for positions.
;;  j0 is the initial value for j.

(define/contract (match-string s t j0)
    (-> string?
        string?
        exact-nonnegative-integer?
        Match?)
    (define m (string-length s))
    (define n (string-length t))
    (define initial-bonus 1)
    (define (loop indexes score bonus i j)
        (if (or (>= i m) (>= j n))
            (Match t (>= i m) (reverse indexes) score)
            (if (equal? (string-ref s i) (string-ref t j))
                (loop
                    (cons j indexes)
                    (+ score bonus)
                    (* bonus 2)
                    (+ i 1)
                    (+ j 1))
                (loop
                    indexes
                    score
                    initial-bonus
                    i
                    (+ j 1))
            )
        )
    )
    (loop '() 0 initial-bonus 0 j0)
)
