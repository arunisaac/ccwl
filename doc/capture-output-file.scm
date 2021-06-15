(define extract
  (command #:run "tar" "--extract" "--file" (input 'archive #:type 'File)
           #:outputs (output 'extracted-file
                             #:type 'File
                             #:binding '((glob . "hello.txt")))))

(workflow ((archive #:type File))
  (extract #:archive archive))
