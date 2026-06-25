;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2026 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (ccwl lang)
  #:declarative? #f
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ccwl conditions)
  #:use-module (ccwl utils)
  #:export (ccwl-read
            ccwl-load))

(define (syntax-list->vector x)
  "Convert syntax @var{x} of a list into syntax of a vector."
  (datum->syntax #f
                 (syntax-case x ()
                   ((elements ...)
                    #'#(elements ...)))
                 #:source x))

(define (read-hash-vector chr port)
  "Read vector from @var{port}. This function is intended for use with
@code{read-hash-extend}."
  (unget-char port chr)
  (let ((lst (read-syntax port)))
    (if (dotted-list? (syntax->datum lst))
        (raise-exception
         (condition (ccwl-violation lst)
                    (formatted-message "Malformed vector: unexpected dotted list ~a"
                                       (syntax->datum lst))))
        (syntax-list->vector lst))))

(read-hash-extend #\( read-hash-vector)

(define* (ccwl-read #:optional (port (current-input-port)))
  "Read an S-expression from @var{port}. This function wraps
@code{read-syntax} to raise read errors the way ccwl likes to handle
it."
  (guard (c ((and (eq? (exception-kind c)
                       'read-error)
                  (message-condition? c)
                  (irritants-condition? c))
             ;; TODO: Once https://codeberg.org/guile/guile/issues/206
             ;; is fixed, report error in source context using
             ;; report-ccwl-violation.
             (raise-exception
              (apply formatted-message
                     (condition-message c)
                     (condition-irritants c)))))
    (read-syntax port)))

(define (ccwl-load file)
  "Load ccwl source @var{file}."
  (let ((source-path (canonicalize-path file)))
    ;; Change directory before loading source file. The source file
    ;; may reference other files with paths relative to its directory.
    (call-with-current-directory (dirname source-path)
      (lambda ()
        (load source-path ccwl-read)))))
