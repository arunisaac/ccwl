;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2023 Arun Isaac <arunisaac@systemreboot.net>
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

(use-modules (srfi srfi-64)
             (term ansi-color))

(define source-in-context
  (@@ (ccwl ui) source-in-context))

(test-begin "ui")

(test-equal "display source in context"
  (string-append "(foo
  "
                 (colorize-string "(bar)" 'BOLD 'RED)
                 ")
")
  (call-with-input-string "(foo
  (bar))
"
    (cut source-in-context <> 1 2)))

(test-end "ui")
