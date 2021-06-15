(define print
  (command #:run "echo" (input 'message #:type 'string)
           #:outputs (output 'printed-message #:type 'stdout)))

(workflow ((message #:type string))
  (print #:message message))
