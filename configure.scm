;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
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

(use-modules (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-28)
             (srfi srfi-37)
             (ice-9 match)
             (ice-9 popen))

(define (call-with-input-pipe command proc)
  "Call PROC with input pipe to COMMAND. COMMAND is a list of program
arguments."
  (match command
    ((prog args ...)
     (let ((port #f))
       (dynamic-wind
         (lambda ()
           (set! port (apply open-pipe* OPEN_READ prog args)))
         (cut proc port)
         (cut close-pipe port))))))

(define (check-for-guile-3.0)
  (display "checking for guile 3.0... ")
  (if (string=? (effective-version) "3.0")
      (display (format "yes~%"))
      (begin
        (display (format "no~%error: Guile 3.0 not found~%"))
        (exit #f))))

(define (check-for-module module)
  (display (format "checking for module ~a... " module))
  (if (resolve-module module #:ensure #f)
      (display (format "yes~%"))
      (begin
        (display (format "no~%error: no ~a found~%" module))
        (exit #f))))

(define (find-program program)
  (find file-exists?
        (map (lambda (path)
               (string-append path "/" program))
             (string-split (getenv "PATH")
                           #\:))))

(define* (check-for-program program #:optional package)
  (display (format "checking for program ~a... " program))
  (let ((found-program (find-program program)))
    (cond
     (found-program
      (display (format "~a~%" found-program)))
     (package
       (display (format "no~%error: cannot find ~a from the ~a package~%"
                        program package))
       (exit #f))
     (else
      (display (format "no~%error: cannot find ~a~%" program))
      (exit #f)))))

(define* (check-for-optional-program program msg)
  (display (format "checking for program ~a... " program))
  (let ((found-program (find-program program)))
    (display (or found-program msg))
    (newline)))

(define (option-proc opt name arg result)
  (cons (cons (string->symbol name) arg)
        result))

(define (unrecognized-option-proc opt name arg result)
  (display (if arg
               (format "Ignoring unrecognized option --~a=~a~%"
                       name arg)
               (format "Ignoring unrecognized option --~a~%"
                       name)))
  result)

(define (unrecognized-argument-proc arg result)
  (display (format "Ignoring unrecognized argument ~a~%"
                   arg))
  result)

(define processed-args
  (match (program-arguments)
    ((_ project args ...)
     (let* ((args (args-fold args
                             (list (option '("prefix") #t #f
                                           option-proc))
                             unrecognized-option-proc
                             unrecognized-argument-proc
                             '()))
            (prefix (or (assq-ref args 'prefix)
                        "/usr/local"))
            (exec-prefix (or (assq-ref args 'exec-prefix)
                             prefix))
            (datarootdir (or (assq-ref args 'datarootdir)
                             (string-append prefix "/share"))))
       `((project . ,project)
         (bindir . ,(or (assq-ref args 'bindir)
                        (string-append exec-prefix "/bin")))
         (libdir . ,(or (assq-ref args 'libdir)
                        (string-append exec-prefix "/lib")))
         (datarootdir . ,datarootdir)
         (infodir . ,(or (assq-ref args 'infodir)
                         (string-append datarootdir "/info"))))))))

(define version
  (call-with-input-pipe (list "git" "tag" "--sort=taggerdate"
                              "--list" "v*")
    (lambda (port)
      (let ((line (get-line port)))
        (if (eof-object? line)
            ;; If there are no tags, assume first version.
            "0.1.0"
            (substring line (string-length "v")))))))

(check-for-guile-3.0)
(check-for-module '(yaml))
(check-for-program "dot" "graphviz")
(check-for-program "cwltool")
(check-for-program "skribilo")
(check-for-optional-program "emacs" "cannot find emacs, and therefore cannot build the website")

(call-with-output-file "Makefile.include"
  (lambda (port)
    (display (format "# This file was automatically generated by configure.

project = ~a
version = ~a
bindir = ~a
libdir = ~a
datarootdir = ~a
infodir = ~a
guile_effective_version = ~a
"
                     (assq-ref processed-args 'project)
                     version
                     (assq-ref processed-args 'bindir)
                     (assq-ref processed-args 'libdir)
                     (assq-ref processed-args 'datarootdir)
                     (assq-ref processed-args 'infodir)
                     (effective-version))
             port)))
