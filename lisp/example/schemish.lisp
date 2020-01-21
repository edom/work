(defmacro define (head &body body)
  "Define in both value namespace and function namespace."
  (cond
    ((symbolp head)
      `(progn
        (defvar ,head ,@body)
        (defun ,head () (error "~S is not a function: ~S" ',head ,head))
        (when (functionp ,head)
          (setf (symbol-function ',head) (function ,head)))))
    ((consp head)
      (let ((name (car head))
            (params (cdr head)))
        `(progn
          (defun ,name ,params ,@body)
          (defvar ,name (function ,name)))))))

(defun println (x) (princ x) (terpri))

;;;;  Examples.

(define x 5)
(define (f x) (1+ x))
(define g (lambda (x) (1+ x)))
(define lst '(1 2 3))
(println (f 1))
(println (funcall f 1)) ;; redundant but no problem
(println (funcall g 2)) ;; the biggest sign of a fake Scheme is FUNCALL
(println (map 'list f '(1 2 3)))

;;  But Racket also has "funcall"; it's just that Racket calls it "#%apply"
;;  and inserts it automatically while expanding modules.
