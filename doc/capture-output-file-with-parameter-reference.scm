(define extract-specific-file
  (command #:inputs (archive #:type File) (extractfile #:type string)
           #:run "tar" "--extract" "--file" archive extractfile
           #:outputs (extracted-file
                      #:type File
                      #:binding '((glob . "$(inputs.extractfile)")))))

(workflow ((archive #:type File) (extractfile #:type string))
  (extract-specific-file #:archive archive #:extractfile extractfile))
