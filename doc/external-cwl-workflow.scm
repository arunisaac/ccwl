(define echo
  (cwl-workflow "echo.cwl"))

(define string-length
  (command #:inputs file
           #:run "wc" "--chars"
           #:outputs (length #:type stdout)
           #:stdin file))

(workflow ((message #:type string))
  (pipe (echo #:message message)
        (string-length #:file output)))
