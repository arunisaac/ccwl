(use-modules (ccwl yaml)
             (srfi srfi-64))

(test-begin "yaml")

(test-equal "dictionary entries with empty arrays and dictionaries for values must render on the same line"
  (scm->yaml-string
   '((foo . #())
     (bar)))
  "foo: []
bar: {}
")

(test-end "yaml")
