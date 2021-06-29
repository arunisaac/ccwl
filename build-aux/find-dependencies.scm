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

;; This script scans source files and prints dependencies for make to
;; pick up.

;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-171)
             (ice-9 match)
             (skribilo reader skribe))

(define (skribilo-document file)
  "Read skribilo FILE and return the document form."
  (call-with-input-file file
    (lambda (port)
      (port-transduce (tfilter (match-lambda
                                 (('document _ ...) #t)
                                 (_ #f)))
                      (rany identity)
                      (make-skribe-reader)
                      port))))

(define (find-dependencies document)
  "Return a list of dependencies of the skribilo form DOCUMENT. Each
dependency is of the form (tag . file). tag may either be the symbol
'image or the symbol 'other."
  (match document
    (('image args ...)
     (apply (lambda* (#:key file #:allow-other-keys)
              `((image . ,file)))
            args))
    (('source args ...)
     (apply (lambda* (#:key file #:allow-other-keys)
              `((other . ,file)))
            args))
    (('scheme-source file)
     `((other . ,file)))
    (('source-ref file _ ...)
     `((other . ,file)))
    ((elements ...)
     (append-map find-dependencies elements))
    (atom '())))

;; Print dependencies
(let ((dependencies
       (find-dependencies (skribilo-document "doc/ccwl.skb"))))
  (format #t "DOC_IMAGES = ~a~%"
          (string-join (filter-map (match-lambda
                                     (('image . file) file)
                                     (_ #f))
                                   dependencies)
                       " "))
  (format #t "DOC_OTHER_DEPENDENCIES = ~a~%"
          (string-join (filter-map (match-lambda
                                     (('other . file) file)
                                     (_ #f))
                                   dependencies)
                       " ")))
