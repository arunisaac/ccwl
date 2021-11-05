(define split-words
  (command #:inputs text
           #:run "tr" "--complement" "--squeeze-repeats" "A-Za-z" "\\n"
           #:stdin text
           #:outputs (words #:type stdout)))

(define downcase
  (command #:inputs words
           #:run "tr" "A-Z" "a-z"
           #:stdin words
           #:outputs (downcased-words #:type stdout)))

(define sort
  (command #:inputs words
           #:run "sort" "--unique"
           #:stdin words
           #:outputs (sorted #:type stdout)))
 
(define find-misspellings
  (command #:inputs words dictionary
           #:run "comm" "-23" words dictionary
           #:outputs (misspellings #:type stdout)))

(workflow (text-file dictionary)
  (pipe (tee (pipe (split-words #:text text-file)
                   (downcase #:words words)
                   (sort (sort-words) #:words downcased-words)
                   (rename #:sorted-words sorted))
             (pipe (sort (sort-dictionary) #:words dictionary)
                   (rename #:sorted-dictionary sorted)))
        (find-misspellings #:words sorted-words
                           #:dictionary sorted-dictionary)))
