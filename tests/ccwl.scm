;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2022, 2023 Arun Isaac <arunisaac@systemreboot.net>
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

(define output
  (@@ (ccwl ccwl) output))

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
    (begin (input #'(message string))
           #f)))

(test-assert "input, when passed an unrecognized keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (input #'(message #:foo string))
           #f)))

(test-assert "input, when passed multiple arguments to a unary keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (input #'(message #:type int string))
           #f)))

(test-assert "output, when passed more than one positional argument, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (output #'(message string))
           #f)))

(test-assert "output, when passed an unrecognized keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (output #'(message #:foo string))
           #f)))

(test-assert "output, when passed multiple arguments to a unary keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (output #'(message #:type int string))
           #f)))

(test-assert "command, when passed positional arguments, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (macroexpand
            '(command foo
                      #:inputs (message #:type string)
                      #:run "echo" message
                      #:outputs (stdout #:type stdout)))
           #f)))

(test-assert "command, when passed an unrecognized keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (macroexpand
            '(command #:foo (message #:type string)
                      #:run "echo" message
                      #:outputs (stdout #:type stdout)))
           #f)))

(test-assert "command, when passed multiple arguments to a unary keyword, must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (macroexpand
            '(command #:inputs (message #:type string)
                      #:run "echo" message
                      #:outputs (stdout #:type stdout)
                      #:stdin "foo" "bar"))
           #f)))

;; TODO: Define this in the lexical scope of the test that requires
;; it.
(define print
  (command #:inputs (message #:type string)
           #:run "echo" message
           #:outputs (printed-message #:type stdout)))

(test-equal "rename should work even on the final output of a workflow"
  (map output-id
       (workflow-outputs
        (workflow ((message1 #:type string)
                   (message2 #:type string))
          (tee (pipe (print (print1) #:message message1)
                     (rename #:out1 printed-message))
               (print (print2) #:message message2)))))
  (list 'out1 'printed-message))

;; TODO: Define this in the lexical scope of the test that requires
;; it.
(define print-with-default
  (command #:inputs (message #:type string #:default "Hello")
           #:run "echo" message
           #:outputs (printed-message #:type stdout)))

(test-assert "allow steps with unspecified default arguments"
  (workflow ()
    (print-with-default)))

(test-assert "allow steps with expressions that evaluate to commands"
  (workflow ((message #:type string))
    ((and #t print)
     (print)
     #:message message)))

(test-assert "step with expression that evaluates to a command but without a step identifier must raise a &ccwl-violation condition"
  (guard (exception
          (else (ccwl-violation? exception)))
    (begin (macroexpand
            '(workflow ((message #:type string))
               ((and #t print)
                #:message message)))
           #f)))

(test-end "ccwl")
