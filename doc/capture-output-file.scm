(define extract
  (command #:inputs (archive #:type 'File)
           #:run "tar" "--extract" "--file" archive
           #:outputs (extracted-file
                      #:type 'File
                      #:binding '((glob . "hello.txt")))))

(workflow ((archive #:type File))
  (extract #:archive archive))
