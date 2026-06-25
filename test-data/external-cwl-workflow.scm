(define echo
  (cwl-workflow "echo.cwl"))

;; TODO: Remove this workflow and return echo directly once bug
;; a9352f5 is fixed.
(workflow ((message #:type string))
  (echo #:message "foo"))
