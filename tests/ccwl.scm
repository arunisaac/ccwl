(use-modules (srfi srfi-71)
             (srfi srfi-64))

(define break-pair
  (module-ref (resolve-module '(ccwl ccwl))
              'break-pair))

(test-begin "ccwl")

(test-assert "break-pair"
  (let ((prefix tail
                (break-pair (case-lambda
                              ((element next)
                               (or (odd? element)
                                   (odd? next)))
                              ((last)
                               (odd? last)))
                            (list 12 66 74 95 7 74 96 46 99 76 37))))
    (equal? prefix (list 12 66))
    (equal? tail (list 74 95 7 74 96 46 99 76 37))))

(test-assert "break-pair: check last elemet handling"
  (let ((prefix tail
                (break-pair (case-lambda
                              ((element next)
                               (or (odd? element)
                                   (odd? next)))
                              ((last)
                               (odd? last)))
                            (list 12 66 74))))
    (equal? prefix (list 12 66 74))
    (equal? tail (list))))

(test-end "ccwl")
