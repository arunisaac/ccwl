(command #:inputs (messages #:type (array string))
         #:run "echo" (array messages #:separator ","))
