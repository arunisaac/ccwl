;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021 Arun I <arunisaac@systemreboot.net>
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

;; A few useful utilities

;;; Code:

(define-module (ccwl utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export (lambda**))

(define* (group-keyword-arguments args #:optional (unary-keywords (list)))
  "Group ARGS, a list of keyword arguments of arbitrary arity. Return
a list of unary keyword arguments. n-ary arguments are grouped
together into lists. Keywords that are to be treated as having unit
arity are listed in UNARY-KEYWORDS."
  (match args
    (((? (lambda (keyword)
           (and (keyword? keyword)
                (not (member keyword unary-keywords))))
         this-keyword)
      tail ...)
     (let ((this-keyword-args tail (break keyword? tail)))
       (cons* this-keyword
              (apply list this-keyword-args)
              (group-keyword-arguments tail unary-keywords))))
    (((? (lambda (keyword)
           (and (keyword? keyword)
                (member keyword unary-keywords)))
         this-keyword)
      this-keyword-arg tail ...)
     (cons* this-keyword this-keyword-arg
            (group-keyword-arguments tail unary-keywords)))
    ((non-keyword _ ...)
     (error "Invalid sequence of keywords arguments" args))
    (() '())))

(define (plist-ref plist key)
  "Return the value from the first entry in PLIST with the given KEY,
or #f if there is no such entry."
  (match (find-tail (cut eq? key <>) plist)
    ((_ value . _) value)
    (#f #f)))

;; TODO: Implement a define** for lambda** in the spirit of define*
;; for lambda*.
(define-syntax lambda**
  (lambda (x)
    "Define a lambda function that can have positional arguments
followed by unary and n-ary keyword arguments. Unary keyword arguments
are prefixed by #:key. n-ary keyword arguments are prefixed by
#:key*. For example:

(lambda** (a b #:key foo #:key* bar)
   (list a b foo bar))

Here, a and b are positional arguments. foo is a unary keyword
argument. bar is an n-ary keyword argument. The above function could,
for example, be invoked as:

((lambda** (a b #:key foo #:key* bar)
   (list a b foo bar))
 1 2 #:foo 123 #:bar 1 2 3)

=> (1 2 123 (1 2 3))"
    (syntax-case x ()
      ((_ args-spec body ...)
       #`(lambda args
           #,(let* ((positionals rest (break keyword? (syntax->datum #'args-spec)))
                    (grouped-rest (group-keyword-arguments rest))
                    (unary-arguments (or (plist-ref grouped-rest #:key)
                                         (list)))
                    (nary-arguments (or (plist-ref grouped-rest #:key*)
                                        (list))))
               #`(apply (lambda* #,(datum->syntax x (append positionals
                                                            (cons #:key (append unary-arguments nary-arguments))))
                          body ...)
                        (let ((positionals rest (break keyword? args)))
                          (append positionals
                                  (group-keyword-arguments
                                   rest (list #,@(map (compose (cut datum->syntax x <>)
                                                               symbol->keyword)
                                                      unary-arguments))))))))))))
