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

(define-module (ccwl conditions)
  #:use-module (rnrs conditions)
  #:use-module (srfi srfi-28)
  #:export (ccwl-violation
            ccwl-violation?
            ccwl-violation-file
            ccwl-violation-line
            ccwl-violation-column))

(define-condition-type &ccwl-violation &violation
  make-ccwl-violation ccwl-violation?
  (file ccwl-violation-file)
  (line ccwl-violation-line)
  (column ccwl-violation-column))

(define (ccwl-violation x)
  "Return &ccwl-violation condition for syntax X."
  (let ((properties (syntax-source x)))
    (make-ccwl-violation (assq-ref properties 'filename)
                         (assq-ref properties 'line)
                         (assq-ref properties 'column))))
