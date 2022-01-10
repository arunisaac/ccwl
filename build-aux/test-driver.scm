;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2022 Arun Isaac <arunisaac@systemreboot.net>
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

;; This is a better test driver for Guile's (srfi srfi-64).
;;
;; TODO: Improve Guile's test driver so this module won't be
;; necessary.

;;; Code:

(use-modules (ice-9 format)
             (ice-9 match)
             (srfi srfi-26)
             (srfi srfi-64))

(define (color code str color?)
  (if color?
      (format #f "~a[~am~a~a[0m" #\esc code str #\esc)
      str))

(define red (cut color 31 <> <>))
(define green (cut color 32 <> <>))
(define magenta (cut color 35 <> <>))

(define (my-gnu-runner color?)
  (let ((runner (test-runner-null)))
    (test-runner-on-group-begin! runner
      (lambda (runner suite-name count)
        (format #t (magenta "%%%% ~a~%" color?) suite-name)))
    (test-runner-on-group-end! runner
      (lambda _
        (newline)))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (let ((name (test-runner-test-name runner))
              (result (string-upcase
                       (symbol->string (test-result-kind runner))))
              (result-alist (test-result-alist runner)))
          (format #t "~a ~a~%"
                  (case (test-result-kind runner)
                    ((pass) (green result color?))
                    (else (red result color?)))
                  name)
          ;; If test did not pass, print details.
          (unless (eq? (test-result-kind runner) 'pass)
            (format (current-error-port)
                    "~a:~a~%expected: ~s~%actual: ~s~%"
                    (assq-ref result-alist 'source-file)
                    (assq-ref result-alist 'source-line)
                    (match (assq-ref result-alist 'source-form)
                      (('test-assert _ ...) #t)
                      (_ (assq-ref result-alist 'expected-value)))
                    (assq-ref result-alist 'actual-value))))))
    runner))

(match (command-line)
  ((_ test-files ...)
   (let ((runner (my-gnu-runner #t)))
     (test-with-runner runner
       (for-each load-from-path test-files)
       (display (magenta "SUMMARY" #t))
       (newline)
       (format #t "PASS: ~a
FAIL: ~a
XPASS: ~a
XFAIL: ~a
SKIP: ~a
"
               (test-runner-pass-count runner)
               (test-runner-fail-count runner)
               (test-runner-xpass-count runner)
               (test-runner-xfail-count runner)
               (test-runner-skip-count runner))
       (exit (zero? (test-runner-fail-count runner)))))))
