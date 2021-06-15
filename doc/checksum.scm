(define md5sum
  (command #:run "md5sum" (input 'file #:type 'File)
           #:outputs (output 'md5 #:type 'stdout)))

(define sha1sum
  (command #:run "sha1sum" (input 'file #:type 'File)
           #:outputs (output 'sha1 #:type 'stdout)))

(define sha256sum
  (command #:run "sha256sum" (input 'file #:type 'File)
           #:outputs (output 'sha256 #:type 'stdout)))

(workflow ((file #:type File))
  (tee (md5sum #:file file)
       (sha1sum #:file file)
       (sha256sum #:file file)))
