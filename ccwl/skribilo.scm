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
  #:use-module (skribilo ast)
  #:use-module (skribilo engine)
  #:use-module (skribilo lib)
  #:use-module (skribilo package base)
  #:use-module (skribilo source lisp)
  #:use-module (skribilo utils keywords)
  #:use-module (skribilo writer)
  #:export (command file scheme-source))

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
