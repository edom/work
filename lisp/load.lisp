(ql:quickload "uiop")

(let ((args (uiop:command-line-arguments))
      (help "
Warning: No system names given.
Usage: <script> <system>...
"))
  (unless args (princ help))
  (loop for name in args
    unless (string= name "--")
    do (ql:quickload name)))
