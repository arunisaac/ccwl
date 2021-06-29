(define count-bytes
  (command #:inputs (file #:type 'File)
           #:run "wc" "-c"
           #:stdin file))

(workflow ((file #:type File))
  (count-bytes #:file file))
