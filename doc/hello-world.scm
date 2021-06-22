(define print
  (command #:inputs (message #:type 'string)
           #:run "echo" message))

(workflow ((message #:type string))
  (print #:message message))
