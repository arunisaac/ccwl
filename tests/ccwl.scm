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

(use-modules (srfi srfi-64)
             (ccwl ccwl))

(test-begin "ccwl")

(test-assert "stdin input should not have inputBinding"
  (not (assoc-ref
        (assoc-ref
         (assoc-ref
          ((module-ref (resolve-module '(ccwl ccwl))
                       'command->cwl)
           (command #:inputs (file #:type 'File)
                    #:run "wc" "-c"
                    #:stdin file))
          'inputs)
         'file)
        'inputBinding)))

(test-end "ccwl")
