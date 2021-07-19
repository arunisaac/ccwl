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

(use-modules (ccwl yaml)
             (srfi srfi-64))

(test-begin "yaml")

(test-equal "dictionary entries with empty arrays and dictionaries for values must render on the same line"
  "foo: []
bar: {}
"
  (scm->yaml-string
   '((foo . #())
     (bar))))

(test-equal "strings with hyphen characters should be escaped"
  "- \"-1\"
"
  (scm->yaml-string #("-1")))

(test-equal "strings with asterisk characters should be escaped"
  "- \"*foo\"
"
  (scm->yaml-string #("*foo")))

(test-end "yaml")
