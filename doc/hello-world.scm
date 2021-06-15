(define print
  (command #:run "echo" (input 'message #:type 'string)))

(workflow ((message #:type string))
  (print #:message message))
