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

(define-module (ccwl skribilo)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-171)
  #:use-module (skribilo ast)
  #:use-module (skribilo engine)
  #:use-module (skribilo lib)
  #:use-module (skribilo package base)
  #:use-module (skribilo source lisp)
  #:use-module (skribilo utils keywords)
  #:use-module (skribilo writer)
  #:export (command
            file
            scheme-source
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

;; Source links
(define (source-ref file regexp text)
  (call-with-input-file file
    (lambda (port)
      (ref #:url (call-with-input-file file
                   (lambda (port)
                     (match (port-transduce (compose (tenumerate 1)
                                                     (tfilter-map
                                                      (match-lambda
                                                        ((line-number . line)
                                                         (and (string-match regexp line)
                                                              line-number)))))
                                            rcons
                                            get-line
                                            port)
                       ((line-number)
                        (string-append %source-uri-base
                                       file
                                       "#L"
                                       (number->string line-number)))
                       ((line-numbers ...)
                        (error "source-ref: regexp found on multiple lines"
                               regexp line-numbers)))))
           #:text text))))

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
