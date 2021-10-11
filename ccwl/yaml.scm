;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of ccwl.
;;;
;;; ccwl is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ccwl is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ccwl.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements a function scm->yaml to convert a scm tree to
;; YAML. The inverse function yaml->scm is not implemented.
;;
;; If you are interested in writing a proper and complete YAML library
;; with both a scm->yaml and a yaml->scm, please feel free (under the
;; terms of the license mentioned earlier) to steal this code.

;;; Code:

(define-module (ccwl yaml)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ccwl utils)
  #:export (scm->yaml
            scm->yaml-string))

(define (display-atom atom port)
  "Display ATOM in PORT."
  (cond
   ((symbol? atom)
    (display-atom (symbol->string atom) port))
   ((number? atom)
    (display atom port))
   ((string? atom)
    ;; TODO: Implement the complete escape logic as per the YAML
    ;; specification.
    ;; Escape string with double quotes if
    ;; - every character is a digit or period, and the unescaped
    ;; string can therefore be misinterpreted as a number
    ;; - string contains the colon, hyphen or asterisk characters
    (if (or (string-every (char-set-union char-set:digit (char-set #\.)) atom)
            (string-any (char-set #\: #\- #\*) atom))
        (write atom port)
        (display atom port)))
   ((boolean? atom)
    (display (if atom "true" "false") port))
   (else (error "Unknown atom" atom))))

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
       ;; If value is an empty array or dictionary, display it on the
       ;; same line.
       ((or #() ())
        (display " " port)
        (scm->yaml value port level))
       ;; Else, display it on the next line.
       (_
        (newline port)
        (indent-level port (1+ level))
        (scm->yaml value port (1+ level)))))))

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
     (display "[]" port)
     (newline port))
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
