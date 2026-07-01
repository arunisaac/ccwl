(define checksum
  (ccwl-load "checksum.scm"))

(workflow ((input #:type File))
  (pipe (checksum …)
        […]))
