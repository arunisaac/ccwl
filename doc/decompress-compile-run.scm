(define decompress
  (command #:inputs (compressed #:type File)
           #:run "gzip" "--stdout" "--decompress" compressed
           #:outputs (decompressed #:type stdout)))

(define compile
  (command #:inputs (source #:type File)
           #:run "gcc" "-x" "c" source
           #:outputs (executable
                      #:type File
                      #:binding '((glob . "a.out")))))

(define run
  (command #:inputs executable
           #:run executable
           #:outputs (stdout #:type stdout)
           #:stdout "run-output.txt"))

(workflow ((compressed-source #:type File))
  (pipe (decompress #:compressed compressed-source)
        (compile #:source decompressed)
        (run #:executable executable)))
