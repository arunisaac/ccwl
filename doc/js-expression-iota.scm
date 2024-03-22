(define iota
  (js-expression #:inputs (n #:type int)
                 #:expression "$({\"sequence\": Array.from(Array(inputs.n).keys())})"
                 #:outputs (sequence #:type (array int))))

(workflow ((n #:type int))
  (iota #:n n))
