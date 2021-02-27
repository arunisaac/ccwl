;;
;; scm->yaml
;;
;; This file implements a library to convert a scm tree to yaml.

(define-module (ccwl yaml)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (scm->yaml
            scm->yaml-string))

(define (kebab->camel string)
  "Convert STRING from kebab case to CAMEL case."
  (match (string-split string #\-)
    ((head tail ...)
     (string-concatenate
      (cons head (map string-titlecase tail))))))

(define (display-atom atom port)
  "Display ATOM in PORT converting from kebab case to camel case if
ATOM is a symbol."
  (cond
   ((symbol? atom)
    (display (string->symbol (kebab->camel (symbol->string atom))) port))
   ((number? atom)
    (display atom port))
   ((string? atom)
    ;; Escape string with double quotes if
    ;; - every character is a digit or period, and the unescaped
    ;; string can therefore be misinterpreted as a number
    ;; - string contains the colon character
    (if (or (string-every (char-set-union char-set:digit (char-set #\.)) atom)
            (string-any #\: atom))
        (write atom port)
        (display atom port)))
   ((boolean? atom)
    (display (if atom "true" "false") port))
   (else (error "Unknown atom" atom))))

(define (indent-level port level)
  "Emit whitespaces to PORT corresponding to nesting LEVEL."
  (display (make-string (* 2 level) #\space) port))

(define (display-array-element element port level)
  "Display array ELEMENT to PORT at nesting LEVEL."
  (display "- " port)
  (scm->yaml element port (1+ level)))

(define (display-dictionary-entry entry port level)
  "Display dictionary ENTRY to PORT at nesting LEVEL."
  (match entry
    ((key . value)
     (display-atom key port)
     (display ":" port)
     (match value
       ((or #(_ ...)
            ((_ . _) (_ . _) ...))
        (newline port)
        (indent-level port (1+ level))
        (scm->yaml value port (1+ level)))
       (_ (display " " port)
          (scm->yaml value port level))))))

(define* (scm->yaml scm #:optional (port (current-output-port)) (level 0))
  "Convert SCM, an S-expression tree, to YAML and display to
PORT. LEVEL is an internal recursion variable."
  (match scm
    (#(head tail ...)
     (display-array-element head port level)
     (for-each (lambda (element)
                 (indent-level port level)
                 (display-array-element element port level))
               tail))
    (#()
     (display "[]" port))
    ((head tail ...)
     (display-dictionary-entry head port level)
     (for-each (lambda (entry)
                 (indent-level port level)
                 (display-dictionary-entry entry port level))
               tail))
    (()
     (display "{}" port)
     (newline port))
    (symbol
     (display-atom symbol port)
     (newline port))))

(define (scm->yaml-string scm)
  (call-with-output-string
    (cut scm->yaml scm <>)))
