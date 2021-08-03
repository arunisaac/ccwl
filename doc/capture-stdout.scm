(define print
  (command #:inputs (message #:type string)
           #:run "echo" message
           #:outputs (printed-message #:type 'stdout)))

(workflow ((message #:type string))
  (print #:message message))
