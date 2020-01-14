;;;;    Example.

(println 'foo)

(format t "Hello world~%")

(defvar *table* (make-hash-table))

(setf (gethash :foo *table*) 100)
(println *table*)

;;;;    Show the dependency tree of an ASDF system.

;;  We may have an ASDF 3 that is somewhat old.

(defun system-name (system)
  (slot-value system 'asdf/system::name))

(defun system-version (system)
  ;; This is supposed to work in ASDF 3.3?
  ;(version (asdf:system-version system))
  (slot-value system 'asdf/system::version))

(defun dep-name (d)
  (trivia:match d
    ((list :version name _) name)
    ((type string) d)
    (_ (error "Unhandled case ~s" d))))

(defun get-set-hash (key value hash)
  (let ((old-value (gethash key hash)))
    (setf (gethash key hash) value)
    old-value))

(defun show-dependency-tree (name)
  (let* ((visited (make-hash-table)))
    (labels (
      (first-visit (key)
        (not (get-set-hash key t visited)))
      (pad (n)
        (loop for i from 0 to n do (princ " ")))
      (doit (level name)
        (let* ((system (asdf:find-system name))
               (version (system-version system))
               (deps (asdf:system-depends-on system)))
          (format t "~a ~a~%" name version)
          (loop for d in deps
                for name = (dep-name d)
                ;when (first-visit name) ;; Do we want this?
            do (pad level)
               (doit (+ level 1) name)))))
      (doit 0 name))))

(show-dependency-tree "my-system")
