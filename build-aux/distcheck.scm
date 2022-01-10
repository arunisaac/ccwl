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

(import (rnrs io ports)
        (srfi srfi-26)
        (ice-9 eval-string)
        (ice-9 match)
        (ice-9 popen)
        (guix gexp)
        (guix packages)
        (guix scripts)
        (guix store))

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

(define (call-with-archive-file archive file proc)
  "Call PROC with port reading FILE in lzip compressed tar ARCHIVE."
  (call-with-input-pipe (list "tar" "--extract" "--to-stdout"
                              "--file" archive
                              (string-append (basename archive ".tar.lz")
                                             "/" file))
    proc))

(define (package-in-archive dist-archive)
  "Return the package object in guix.scm of DIST-ARCHIVE."
  (call-with-archive-file dist-archive "guix.scm"
    (lambda (port)
      (eval-string (get-string-all port)
                   #:file (string-append (getcwd) "/guix.scm")))))

(define (check-build dist-archive)
  "Check if package in DIST-ARCHIVE builds correctly."
  (run-with-store (open-connection)
    (build-package
     (package
       (inherit (package-in-archive dist-archive))
       (source (local-file dist-archive))))))

(define (check-news version dist-archive)
  "Check if VERSION appears in the first 200 characters of the news
file in DIST-ARCHIVE."
  (call-with-archive-file dist-archive "NEWS.org"
    (lambda (port)
      (unless (string-contains (get-string-n port 200)
                               version)
        (error "NEWS.org does not mention current version:" version)))))

(match (program-arguments)
  ((_ version dist-archive)
   (check-build dist-archive)
   (check-news version dist-archive)))
