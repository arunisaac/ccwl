;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2022, 2023 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (ccwl ui)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-28)
  #:use-module (term ansi-color)
  #:use-module (ccwl conditions)
  #:export (report-formatted-message
            report-ccwl-violation))

(define (report-formatted-message exception)
  "Report @var{exception}, a @code{&formatted-message} condition to the
user."
  (display (apply format
                  (formatted-message-format exception)
                  (map (cut colorize-string <> 'BOLD 'MAGENTA)
                       (formatted-message-arguments exception)))
           (current-error-port))
  (newline (current-error-port)))

(define (repeat thunk n)
  "Call THUNK N times."
  (unless (zero? n)
    (thunk)
    (repeat thunk (1- n))))

(define (save-excursion thunk port)
  "Call THUNK and restore PORT position to what it was before THUNK
was executed. For the curious, this function is named after the
save-excursion function in elisp."
  (let ((original-position #f))
    (dynamic-wind (lambda ()
                    (set! original-position (port-position port)))
                  thunk
                  (lambda ()
                    (set-port-position! port original-position)))))

(define (read-end port)
  "Return the position in PORT corresponding to the end of the
S-expression starting at the current port position. This function does
not affect the current port position."
  (save-excursion (lambda ()
                    (read port)
                    (port-position port))
                  port))

(define (read-sexp-string port)
  "Read an S-expression from PORT and return it as a string with
whitespace intact."
  (get-string-n port (- (read-end port)
                        (port-position port))))

(define (count-lines str)
  "Count the number of lines in STR."
  (call-with-input-string str
    (lambda (port)
      (let loop ()
        (if (eof-object? (get-line port))
            0
            (1+ (loop)))))))

(define (number-of-digits n)
  "Return the number of decimal digits in positive integer N."
  (if (< n 10)
      1
      (1+ (number-of-digits (quotient n 10)))))

(define (display-integer n minimum-width port)
  "Display integer N to PORT using at least MINIMUM-WIDTH
characters. If N is not large enough, it is padded with spaces."
  (display (make-string (max 0 (- minimum-width
                                  (number-of-digits n)))
                        #\space)
           port)
  (display n port))

(define (display-with-line-numbers str out starting-line-number)
  "Display STR to port OUT with each line prefixed with a line
number. Line numbers start from STARTING-LINE-NUMBER."
  (call-with-input-string str
    (lambda (in)
      (let ((last-line-number (+ starting-line-number
                                 (count-lines str))))
        (let loop ((line-number starting-line-number))
          (let ((line (get-line in)))
            (unless (eof-object? line)
              (display (make-string 4 #\space) out)
              (display-integer line-number
                               (number-of-digits last-line-number)
                               out)
              (display (format " | ~a~%" line) out)
              (loop (1+ line-number)))))))))

(define (put-line line port)
  "Display LINE to PORT followed by a newline character."
  (display line port)
  (newline port))

(define (string-blank? str)
  "Return non-#f if STR contains only whitespace characters, else
return #t."
  (string-every char-set:whitespace str))

(define (source-in-context port line-number column-number)
  "Return source from PORT at LINE-NUMBER, COLUMN-NUMBER in context
with S-expression at LINE-NUMBER, COLUMN-NUMBER highlit in
red. LINE-NUMBER and COLUMN-NUMBER are zero-based."
  (call-with-output-string
    (lambda (out)
      ;; Unless the 0th line is requested, display line preceding
      ;; syntax x.
      (unless (zero? line-number)
        ;; Get to line preceding syntax x.
        (repeat (cut get-line port)
                (1- line-number))
        ;; Display line preceding syntax x unless blank.
        (let ((line (get-line port)))
          (unless (string-blank? line)
            (put-line line out))))
      ;; Display part of line before syntax x.
      (display (get-string-n port column-number)
               out)
      ;; Display syntax x in red.  Color each line separately to
      ;; help line oriented functions like
      ;; `display-with-line-numbers'.
      (display (string-join (map (cut colorize-string <> 'BOLD 'RED)
                                 (string-split (read-sexp-string port)
                                               #\newline))
                            "\n")
               out)
      ;; Display part of line after syntax x.
      (put-line (get-line port) out)
      ;; Display line following syntax x unless blank.
      (let ((line (get-line port)))
        (unless (or (eof-object? line)
                    (string-blank? line))
          (put-line line out))))))

(define (report-ccwl-violation exception)
  (let ((file (ccwl-violation-file exception))
        (line (ccwl-violation-line exception))
        (column (ccwl-violation-column exception)))
    (display (colorize-string (format "~a:~a:~a: " file (1+ line) column)
                              'BOLD)
             (current-error-port))
    (display (colorize-string "error:" 'BOLD 'RED)
             (current-error-port))
    (display " " (current-error-port))
    (when (formatted-message? exception)
      (report-formatted-message exception))
    (display-with-line-numbers (call-with-input-file file
                                 (cut source-in-context <> line column))
                               (current-error-port)
                               (max 1 line))))
