(define print-message
  (command #:inputs (message #:type string)
           #:run "echo" message
           #:outputs (printed-message #:type stdout)))

(define print-file
  (command #:inputs (file #:type File)
           #:run "cat" file
           #:outputs (printed-file #:type stdout)))

(workflow ((message #:type string))
  (pipe (print-message #:message message)
        (tee (print-file #:file printed-message)
             (identity))))
