;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2023, 2025 Arun Isaac <arunisaac@systemreboot.net>
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

(use-modules (srfi srfi-64))

(define make-input
  (@@ (ccwl ccwl) make-input))

(define input->cwl-scm
  (@@ (ccwl cwl) input->cwl-scm))

(define type->cwl
  (@@ (ccwl cwl) type->cwl))

(define make-array-type
  (@@ (ccwl ccwl) make-array-type))

(test-begin "cwl")

(test-equal "type->cwl on primitive types"
  'File
  (type->cwl 'File))

(test-equal "type->cwl on array types"
  '((type . array)
    (items . File))
  (type->cwl (make-array-type 'File)))

(test-equal "type->cwl on nested array types"
  '((type . array)
    (items . ((type . array)
              (items . File))))
  (type->cwl (make-array-type (make-array-type 'File))))

(test-equal "Serialize #f defaults in input values"
  '("foo"
    (type . boolean)
    (default . #f)
    (label . "foo"))
  (input->cwl-scm
   (make-input "foo" 'boolean "foo" #f #f #f #f '())))

(test-end "cwl")
