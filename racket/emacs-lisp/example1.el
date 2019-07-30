(progn
    (fset 'f (lambda (x) (+ x 1)))
    (fset 'g 'f)
    (setq a 1)
    (setq b 2)
    (print (+ a b))
    (print (f 5))
)
