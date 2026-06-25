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

(define-module (test-utils utils)
  #:use-module (rnrs exceptions)
  #:use-module (srfi srfi-64)
  #:export (test-condition))

(define-syntax-rule (test-condition test-name condition-predicate test-expression)
  (test-assert test-name
    (guard (condition
            (else (condition-predicate condition)))
      (begin test-expression
             #f))))
