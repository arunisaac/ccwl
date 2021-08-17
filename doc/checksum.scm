(define md5sum
  (command #:inputs (file #:type File)
           #:run "md5sum" file
           #:outputs (md5 #:type stdout)))

(define sha1sum
  (command #:inputs (file #:type File)
           #:run "sha1sum" file
           #:outputs (sha1 #:type stdout)))

(define sha256sum
  (command #:inputs (file #:type File)
           #:run "sha256sum" file
           #:outputs (sha256 #:type stdout)))

(workflow ((file #:type File))
  (tee (md5sum #:file file)
       (sha1sum #:file file)
       (sha256sum #:file file)))
