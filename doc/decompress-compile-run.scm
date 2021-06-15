(define decompress
  (command #:run "gzip" "--stdout" "--decompress" (input 'compressed #:type 'File)
           #:outputs (output 'decompressed #:type 'stdout)))

(define compile
  (command #:run "gcc" "-x" "c" (input 'source #:type 'File)
           #:outputs (output 'executable
                             #:type 'File
                             #:binding '((glob . "a.out")))))

(define run
  (command #:run (input 'executable)
           #:outputs (output 'stdout #:type 'stdout)))

(workflow ((compressed-source #:type File))
  (pipe (decompress #:compressed compressed-source)
        (compile #:source decompressed)
        (run #:executable executable)))
