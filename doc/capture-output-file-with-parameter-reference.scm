(define extract-specific-file
  (command #:run "tar" "--extract" "--file" (input 'archive #:type 'File)
                                            (input 'extractfile #:type 'string)
           #:outputs (output 'extracted-file
                             #:type 'File
                             #:binding '((glob . "$(inputs.extractfile)")))))

(workflow ((archive #:type File) (extractfile #:type string))
  (extract-specific-file #:archive archive #:extractfile extractfile))
