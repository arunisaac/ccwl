;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2022 Arun Isaac <arunisaac@systemreboot.net>
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

(use-modules (rnrs exceptions)
             (srfi srfi-64)
             (ccwl ccwl)
             (ccwl conditions))

(define input
  (@@ (ccwl ccwl) input))

(test-begin "ccwl")

(test-assert "stdin input should not have inputBinding"
  (not (assoc-ref
        (assoc-ref
         (assoc-ref
          ((@@ (ccwl cwl) command->cwl-scm)
           (command #:inputs (file #:type File)
                    #:run "wc" "-c"
                    #:stdin file))
          'inputs)
         'file)
        'inputBinding)))

(test-equal "read all forms of inputs and outputs from a CWL workflow"
  '(((spam string))
    ((ham stdout)
     (eggs stdout)))
  (let ((cwl-workflow (cwl-workflow "tests/input-output-parameters.cwl")))
    (list (map (lambda (input)
                 (list (input-id input)
                       (input-type input)))
               (cwl-workflow-inputs cwl-workflow))
          (map (lambda (output)
                 (list (output-id output)
                       (output-type output)))
               (cwl-workflow-outputs cwl-workflow)))))

(test-assert "input, when passed more than one positional argument, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (input #'(message string))))

(test-assert "input, when passed an unrecognized keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (input #'(message #:foo string))))

(test-assert "input, when passed multiple arguments to a unary keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (input #'(message #:type int string))))

(test-end "ccwl")
