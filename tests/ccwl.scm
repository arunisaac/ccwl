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
             (srfi srfi-1)
             (srfi srfi-64)
             (srfi srfi-71)
             (ccwl ccwl)
             (ccwl conditions))

(define input
  (@@ (ccwl ccwl) input))

(define output
  (@@ (ccwl ccwl) output))

(define make-array-type
  (@@ (ccwl ccwl) make-array-type))

(define key
  (@@ (ccwl ccwl) key))

(define collect-steps
  (@@ (ccwl ccwl) collect-steps))

(define-syntax construct-type-syntax-wrapper
  (lambda (x)
    (syntax-case x ()
      ((_ type-spec)
       ((@@ (ccwl ccwl) construct-type-syntax)
        #'type-spec)))))

(define-syntax-rule (test-condition test-name condition-predicate test-expression)
  (test-assert test-name
    (guard (condition
            (else (condition-predicate condition)))
      (begin test-expression
             #f))))

(define (ccwl-violation-with-message? message)
  (lambda (condition)
    (and (ccwl-violation? condition)
         (string=? (formatted-message-format condition)
                   message))))

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

(test-condition "input, when passed more than one positional argument, must raise a &ccwl-violation condition"
  ccwl-violation?
  (input #'(message string)))

(test-condition "input, when passed an unrecognized keyword, must raise a &ccwl-violation condition"
  ccwl-violation?
  (input #'(message #:foo string)))

(test-condition "input, when passed multiple arguments to a unary keyword, must raise a &ccwl-violation condition"
  ccwl-violation?
  (input #'(message #:type int string)))

(test-condition "output, when passed more than one positional argument, must raise a &ccwl-violation condition"
  ccwl-violation?
  (output #'(message string)))

(test-condition "output, when passed an unrecognized keyword, must raise a &ccwl-violation condition"
  ccwl-violation?
  (output #'(message #:foo string)))

(test-condition "output, when passed multiple arguments to a unary keyword, must raise a &ccwl-violation condition"
  ccwl-violation?
  (output #'(message #:type int string)))

(test-condition "command, when passed positional arguments, must raise a &ccwl-violation condition"
  ccwl-violation?
  (macroexpand
   '(command foo
             #:inputs (message #:type string)
             #:run "echo" message
             #:outputs (stdout #:type stdout))))

(test-condition "command, when passed an unrecognized keyword, must raise a &ccwl-violation condition"
  ccwl-violation?
  (macroexpand
   '(command #:foo (message #:type string)
             #:run "echo" message
             #:outputs (stdout #:type stdout))))

(test-condition "command, when passed multiple arguments to a unary keyword, must raise a &ccwl-violation condition"
  ccwl-violation?
  (macroexpand
   '(command #:inputs (message #:type string)
             #:run "echo" message
             #:outputs (stdout #:type stdout)
             #:stdin "foo" "bar")))

;; TODO: Define this in the lexical scope of the test that requires
;; it.
(define print
  (command #:inputs (message #:type string)
           #:run "echo" message
           #:outputs (printed-message #:type stdout)))

(test-equal "rename should work even on the final output of a workflow"
  (list 'printed-message 'out1)
  (map output-id
       (workflow-outputs
        (workflow ((message1 #:type string)
                   (message2 #:type string))
          (tee (pipe (print (print1) #:message message1)
                     (rename #:out1 printed-message))
               (print (print2) #:message message2))))))

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

(test-condition "step with expression that evaluates to a command but without a step identifier must raise a &ccwl-violation condition"
  ccwl-violation?
  (macroexpand
   '(workflow ((message #:type string))
      ((and #t print)
       #:message message))))

(test-assert "allow literal strings as arguments"
  (workflow ()
    (print #:message "Hello")))

;; TODO: Define this in the lexical scope of the test that requires
;; it.
(define print-int
  (command #:inputs (number #:type int)
           #:run "echo" number
           #:outputs (printed-number #:type stdout)))

(test-assert "allow literal ints as arguments"
  (workflow ()
    (print-int #:number 42)))

(test-condition "step supplied with an unknown key must raise a &ccwl-violation condition"
  ccwl-violation?
  (macroexpand
   '(workflow ((message #:type string))
      (print #:message mess))))

(test-condition "unrecognized workflow syntaxes must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Unrecognized workflow syntax [expected (workflow (input ...) tree)]")
  (macroexpand
   '(workflow foo ((message #:type string))
              (print #:message message))))

(test-condition "multiple expressions in workflow body must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "More than one expression ~a in workflow body. Perhaps you need to combine them with a pipe or a tee?")
  (macroexpand
   '(workflow ((message1 #:type string)
               (message2 #:type string))
      (print (print1) #:message message1)
      (print (print2) #:message message2))))

(test-condition "commands with non-string #:stderr parameters must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Invalid #:stderr parameter ~a. #:stderr parameter must be a string")
  (macroexpand
   '(command #:inputs (message #:type string)
             #:run "echo" message
             #:outputs (printed #:type stderr)
             #:stderr captured-stderr)))

(test-condition "commands with non-string #:stdout parameters must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Invalid #:stdout parameter ~a. #:stdout parameter must be a string")
  (macroexpand
   '(command #:inputs (message #:type string)
             #:run "echo" message
             #:outputs (printed #:type stdout)
             #:stdout captured-stdout)))

(test-condition "command definitions with undefined inputs in their #:run arguments must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Undefined input ~a")
  (macroexpand
   '(command #:inputs (number #:type int)
             #:run "echo" n)))

(test-condition "command definitions with undefined prefix inputs in their #:run arguments must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Undefined input ~a")
  (macroexpand
   '(command #:inputs (number #:type int)
             #:run "echo" ("-x" n))))

(test-condition "command definitions with invalid #:run arguments must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Invalid command element ~a. Command elements must either be input identifiers or literal strings.")
  (macroexpand
   '(command #:run "echo" 42)))

(test-assert "tolerate prefixed string arguments in command definitions"
  (command #:run "echo" ("-x" "foo")))

(test-condition "command definitions with non-string prefixes in prefixed inputs must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Invalid prefix ~a. Prefixes must be strings.")
  (macroexpand
   '(command #:inputs (number #:type int)
             #:run "echo" (-x number))))

(test-condition "inputs with an invalid #:stage? parameter must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Invalid #:stage? parameter ~a. #:stage? must either be #t or #f.")
  (macroexpand
   '(command #:inputs (file #:type File
                            #:stage? 42)
             #:run "cat" file)))

(test-condition "inputs with #:other parameters that fail to evaluate must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "#:other parameter not serializable to YAML")
  (macroexpand
   '(command #:inputs (file #:type File
                            #:other '((secondaryFiles . ".fai")))
             #:run "cat" file)))

(test-condition "outputs with #:other parameters that fail to evaluate must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "#:other parameter not serializable to YAML")
  (macroexpand
   '(command #:outputs (file #:type File
                             #:other '((secondaryFiles . ".fai")))
             #:run "cat" file)))

(test-condition "commands with #:other parameters that fail to evaluate must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "#:other parameter not serializable to YAML")
  (macroexpand
   '(command #:run "cat" file
             #:other '((secondaryFiles . ".fai")))))

(test-eq "construct-type-syntax on primitive types"
  'File
  (construct-type-syntax-wrapper File))

(test-eq "construct-type-syntax on array types"
  (make-array-type 'File)
  (construct-type-syntax-wrapper (array File)))

(test-eq "construct-type-syntax on nested array types"
  (make-array-type (make-array-type 'File))
  (construct-type-syntax-wrapper (array (array File))))

(test-condition "rename with non-keyword arguments must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Expected keyword (for example: #:foo, #:bar)")
  (macroexpand
   '(workflow ((message #:type string))
      (rename (foo) #:foo message))))

(test-condition "rename with unknown key must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Unknown key ~a. Known keys at this step are ~a.")
  (macroexpand
   '(workflow ((foo #:type string))
      (rename #:bar foobar))))

(test-condition "commands with non-string #:separator parameters must raise a &ccwl-violation condition"
  (ccwl-violation-with-message?
   "Invalid #:separator parameter ~a. #:separator parameter must be a string.")
  (macroexpand
   '(command #:inputs (messages #:type (array string))
             #:run "echo" (array messages #:separator foo))))

(test-assert "tee must deduplicate global workflow input keys"
  (let ((keys steps (collect-steps #'(tee (print #:message message)
                                          (identity))
                                   (list (key 'message)))))
    (= (length (delete-duplicates keys eq?))
       (length keys))))

(test-end "ccwl")
