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

;; This module customizes Skribilo to our needs.

;;; Code:

(define-module (doc skribilo)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-171)
  #:use-module (skribilo ast)
  #:use-module (skribilo engine)
  #:use-module (skribilo lib)
  #:use-module (skribilo package base)
  #:use-module (skribilo parameters)
  #:use-module (skribilo source lisp)
  #:use-module (skribilo utils keywords)
  #:use-module (skribilo writer)
  #:export (command
            file
            scheme-source
            scheme-source-form
            source-ref))

;; Constants
(define %source-uri-base
  "https://github.com/arunisaac/ccwl/blob/main/")

;; Aliases
(define file samp)
(define command code)

;; Shorthand for scheme source
(define (scheme-source file)
  (prog (source #:language scheme #:file file)
        #:line #f))

;; Abbreviations
(define-markup (abbr #:rest opts
		     #:key (ident #f) (class "abbr") (short #f) (long #f))
  (new container
       (markup 'abbr)
       (ident (or ident (symbol->string (gensym "abbr"))))
       (class class)
       (loc &invocation-location)
       (required-options '(#:short #:long))
       (options `((#:short ,short)
		  (#:long ,long)
		  ,@(the-options opts #:ident #:class #:short #:long)))
       (body (the-body opts))))

;; S-exp source links
(define (sexp-position str regexp)
  "Return (START . END) where START is the start of the match to
REGEXP in STR and END is the end of the sexp beginning at START. START
and END are character positions indexed from 0. If multiple matches
are found, error out."
  (cond
   ((string-match regexp str)
    => (lambda (match-struct)
         (let ((start (match:start match-struct)))
           (if (string-match regexp (substring str (1+ start)))
               (error "source-ref: regexp found on multiple lines" regexp)
               (cons start
                     (1- (- (string-length str)
                            (string-length
                             (call-with-input-string (substring str start)
                               (lambda (port)
                                 (read port)
                                 (get-string-all port)))))))))))
   (else
    (error "source-sexp-ref: regexp not found" regexp))))

(define (position->line-number str position)
  "Return the line number in STR corresponding to POSITION."
  (string-fold (lambda (c result)
                 (if (char=? c #\newline)
                     (1+ result)
                     result))
               1
               (substring str 0 position)))

(define (sexp-file-lines file regexp)
  "Return (START . END) where START is the start of the match to
REGEXP in STR and END is the end of the sexp beginning at START. START
and END are line numbers indexed from 1."
  (let ((str (call-with-input-file file get-string-all)))
    (match (sexp-position str regexp)
      ((start . end)
       (cons (position->line-number str start)
             (position->line-number str end))))))

(define (source-ref file regexp text)
  (ref #:url (match (sexp-file-lines (search-path (*source-path*) file)
                                     regexp)
               ((start-line . end-line)
                (if (= start-line end-line)
                    (string-append %source-uri-base
                                   file
                                   "#L"
                                   (number->string start-line))
                    (string-append %source-uri-base
                                   file
                                   "#L"
                                   (number->string start-line)
                                   "-L"
                                   (number->string end-line)))))
       #:text text))

;; Extract forms from scheme source
(define (scheme-source-form file regexp)
  "Extract form from scheme source FILE whose beginning matches
REGEXP. Return it enclosed in a prog form."
  (prog (match (sexp-file-lines file regexp)
          ((start . stop)
           (source #:language scheme
                   #:file file
                   #:start (1- start)
                   #:stop (1- stop))))
        #:line #f))

;; HTML engine customizations
(let ((html-engine (find-engine 'html)))
  (engine-custom-set! html-engine 'css "/style.css")
  (engine-custom-set! html-engine 'charset "UTF-8")
  (markup-writer 'abbr html-engine
                 #:options '(#:short #:long)
                 #:action (lambda (markup engine)
                            (format #t "<abbr title=\"~a\">~a</abbr> (~a)"
                                    (markup-option markup #:long)
                                    (markup-option markup #:short)
                                    (markup-option markup #:long)))))
