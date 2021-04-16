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

(use-modules (srfi srfi-71)
             (srfi srfi-64))

(define break-pair
  (module-ref (resolve-module '(ccwl ccwl))
              'break-pair))

(test-begin "ccwl")

(test-assert "break-pair"
  (let ((prefix tail
                (break-pair (case-lambda
                              ((element next)
                               (or (odd? element)
                                   (odd? next)))
                              ((last)
                               (odd? last)))
                            (list 12 66 74 95 7 74 96 46 99 76 37))))
    (equal? prefix (list 12 66))
    (equal? tail (list 74 95 7 74 96 46 99 76 37))))

(test-assert "break-pair: check last elemet handling"
  (let ((prefix tail
                (break-pair (case-lambda
                              ((element next)
                               (or (odd? element)
                                   (odd? next)))
                              ((last)
                               (odd? last)))
                            (list 12 66 74))))
    (equal? prefix (list 12 66 74))
    (equal? tail (list))))

(test-end "ccwl")
