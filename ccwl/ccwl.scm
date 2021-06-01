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

;;; Commentary:

;; This is the main module that presents the public interface to ccwl.

;;; Code:

(define-module (ccwl ccwl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ccwl utils)
  #:use-module (ccwl yaml)
  #:export (command
            workflow
            input
            output
            step
            pipeline
            write-cwl))

(define %cwl-version "v1.2")

(define-immutable-record-type <input>
  (make-input id type label default source prefix other)
  input?
  (id input-id)
  (type input-type)
  (label input-label)
  (default input-default)
  (source input-source set-input-source)
  (prefix input-prefix)
  (other input-other))

(define-immutable-record-type <unspecified-default>
  (make-unspecified-default)
  unspecified-default?)

(define* (input id #:key (type 'File) label (default (make-unspecified-default)) (other '()))
  "Build and return an <input> object."
  ;; The user should never set source, and should not set prefix
  ;; directly during construction of the <input> object. Hence, do not
  ;; expose it as a parameter of this constructor.
  (let ((source #f)
        (prefix #f))
    (make-input id type label default source prefix other)))

(define-immutable-record-type <output>
  (make-output id type binding source other)
  output?
  (id output-id)
  (type output-type)
  (binding output-binding)
  (source output-source set-output-source)
  (other output-other))

(define* (output id #:key (type 'File) binding source (other '()))
  "Build and return an <output> object."
  (make-output id type binding source other))

(define (filter-alist alist)
  "Filter ALIST removing entries with #f as the value."
  (filter (match-lambda
            ((_ . #f) #f)
            (_ #t))
          alist))

(define-immutable-record-type <step>
  (make-step id run in out)
  step?
  (id step-id)
  (run step-run)
  (in step-in)
  (out step-out))

(define-immutable-record-type <command>
  (make-command additional-inputs outputs args stdin other)
  command?
  (additional-inputs command-additional-inputs)
  (outputs command-outputs set-command-outputs)
  (args command-args)
  (stdin command-stdin set-command-stdin)
  (other command-other))

(define command
  (lambda** (#:key stdin #:key* run (additional-inputs '()) (outputs '()) (other '()))
    (make-command additional-inputs outputs run stdin other)))

(define (input=? input1 input2)
  (eq? (input-id input1)
       (input-id input2)))

(define (invoke-command step-id command . args)
  (make-step step-id
             command
             (plist->alist args)
             (command-outputs command)))

(define* (make-workflow steps inputs outputs #:key (other '()))
  "Build a Workflow class CWL workflow."
  `((cwlVersion . ,%cwl-version)
    (class . Workflow)
    (requirements (SubworkflowFeatureRequirement))
    ,@other
    (inputs . ,(map (lambda (input)
                      `(,(input-id input)
                        ,@(filter-alist
                           `((type . ,(input-type input))
                             (label . ,(input-label input))
                             (default . ,(and (not (unspecified-default? (input-default input)))
                                              (input-default input)))))
                        ,@(input-other input)))
                    inputs))
    (outputs . ,(map (lambda (output)
                       `(,(output-id output)
                         (type . ,(match (output-type output)
                                    ('stdout 'File)
                                    (some-other-type some-other-type)))
                         (outputSource . ,(match (output-source output)
                                            ((? string? source) source)
                                            ((? input? input) (input-id input))))))
                     outputs))
    (steps . ,(map (lambda (step)
                     `(,(step-id step)
                       (in . ,(map (lambda (in)
                                     (match in
                                       ((id . (? string? source))
                                        in)
                                       ((id . (? input? input))
                                        (cons id (input-id input)))))
                                   (step-in step)))
                       (out . ,(list->vector (map output-id (step-out step))))
                       (run . ,(match (step-run step)
                                 ((? command? command)
                                  (command->cwl command))
                                 (tree tree)))))
                   steps))))

(define (output->cwl output)
  `(,(output-id output)
    ,@(filter identity
              (list (and (output-type output)
                         (cons 'type (output-type output)))
                    (and (output-binding output)
                         (cons 'outputBinding (output-binding output)))))
    ,@(output-other output)))

(define-immutable-record-type <cli-element>
  (make-cli-element argument position)
  cli-element?
  (argument cli-element-argument)
  (position cli-element-position))

(define (command->cwl command)
  (let ((elements
         ;; Add a position to all arguments, converting them to
         ;; <cli-element> objects.
         (map (lambda (arg position)
                (make-cli-element
                 ;; If duplicate input, convert it to an expression
                 ;; referring to the input.
                 (if (and (input? arg)
                          (not (= (list-index (lambda (x)
                                                (and (input? x)
                                                     (input=? arg x)))
                                              (command-args command))
                                  position)))
                     (string-append "$(inputs." (symbol->string (input-id arg)) ")")
                     arg)
                 position))
              (command-args command)
              (iota (length (command-args command))))))
    `((cwlVersion . ,%cwl-version)
      (class . CommandLineTool)
      ,@(command-other command)
      (arguments . ,(list->vector
                     ;; Put string arguments into the arguments array.
                     (filter-map (lambda (element)
                                   (and (string? (cli-element-argument element))
                                        `((position . ,(cli-element-position element))
                                          (valueFrom . ,(cli-element-argument element)))))
                                 elements)))
      (inputs . ,(append
                  ;; Put <input> arguments into the inputs array.
                  (filter-map (lambda (element)
                                (let ((input (cli-element-argument element)))
                                  (and (input? input)
                                       `(,(input-id input)
                                         ,@(filter-alist
                                            `((type . ,(input-type input))
                                              (label . ,(input-label input))
                                              (default . ,(and (not (unspecified-default? (input-default input)))
                                                               (input-default input)))
                                              (inputBinding . ,(filter-alist
                                                                `((position . ,(cli-element-position element))
                                                                  (prefix . ,(input-prefix input)))))))
                                         ,@(input-other input)))))
                              elements)
                  (map (lambda (input)
                         `(,(input-id input)
                           ,@(filter-alist
                              `((type . ,(input-type input))
                                (label . ,(input-label input))
                                (default . ,(and (not (unspecified-default? (input-default input)))
                                                 (input-default input)))))
                           ,@(input-other input)))
                       (command-additional-inputs command))
                  (let ((stdin (command-stdin command)))
                    (if stdin
                        (list `(,(input-id stdin)
                                (type . ,(input-type stdin))))
                        (list)))))
      (outputs . ,(map output->cwl (command-outputs command)))
      ,@(if (command-stdin command)
            `((stdin . ,(string-append "$(inputs."
                                       (symbol->string
                                        (input-id (command-stdin command)))
                                       ".path)")))
            '()))))

(define (write-cwl step file)
  (call-with-output-file file
    (cut scm->yaml (let ((run (step-run step)))
                     (if (command? run)
                         (command->cwl run)
                         run))
         <>)))

(define (command-input-keys command)
  "Return the list of input keys accepted by COMMAND."
  (map input-id
       (append (filter input? (command-args command))
               (command-additional-inputs command)
               (cond ((command-stdin command) => list)
                     (else (list))))))

(define-immutable-record-type <key>
  (make-key name step)
  key?
  (name key-name)
  (step key-step))

(define* (key name #:optional step)
  "Build and return a <key> object."
  (make-key name step))

;; TODO: Add docstring.
(define (cwl-key-address key)
  (if (key-step key)
      ;; Input/output of particular step
      (string-append (symbol->string (key-step key))
                     "/" (symbol->string (key-name key)))
      ;; Global input/output
      (symbol->string (key-name key))))

(define (command-object command-syntax)
  "Return the command object described by COMMAND-SYNTAX. If such a
command is not defined, return #f."
  (let ((var (module-variable (current-module)
                              (syntax->datum command-syntax))))
    (and var
         (command? (variable-ref var))
         (variable-ref var))))

(define (workflow-steps x input-keys)
  "Traverse ccwl source X and return list of steps. INPUT-KEYS is a
list of supplied input <key> objects."
  (syntax-case x (pipe tee)
    ;; pipe
    ((pipe expressions ...)
     (foldn (lambda (expression input-keys steps)
              (let ((child-output-keys child-steps (workflow-steps expression input-keys)))
                (values (append input-keys child-output-keys)
                        (append steps child-steps))))
            #'(expressions ...)
            input-keys
            (list)))
    ;; tee
    ((tee expressions ...)
     (append-mapn (cut workflow-steps <> input-keys)
                  #'(expressions ...)))
    ;; commands with only a single input and when only a single key is
    ;; available at this step
    ((command (step-id))
     (and (command-object #'command)
          (= (length input-keys) 1)
          (= (length (command-input-keys
                      (command-object #'command)))
             1))
     (workflow-steps #`(command (step-id)
                                #,(match (command-input-keys
                                          (command-object #'command))
                                    ((command-key) (symbol->keyword command-key)))
                                #,(match input-keys
                                    ((input-key) (key-name input-key))))
                     input-keys))
    ((command (step-id) args ...)
     ;; Run a whole bunch of tests so that we can produce useful error
     ;; messages.
     (let ((input-key-symbols (map key-name input-keys))
           (command-object (command-object #'command))
           (step-id (syntax->datum #'step-id)))
       ;; Test for undefined command.
       (unless command-object
         (error "Undefined ccwl command:" (syntax->datum #'command)))
       ;; Test for missing required parameters.
       ;; TODO: Filter out optional parameters.
       (match (lset-difference
               eq?
               (command-input-keys command-object)
               (map (match-lambda
                      ((key . _) (keyword->symbol key)))
                    (syntax->datum (pairify #'(args ...)))))
         (() #t)
         (missing-parameters
          (scm-error 'misc-error
                     #f
                     "Step ~S missing required parameters ~S"
                     (list step-id missing-parameters)
                     #f)))
       ;; Test for unknown keys.
       (for-each (match-lambda
                   ((arg . value)
                    (unless (memq (keyword->symbol arg)
                                  (command-input-keys command-object))
                      (scm-error 'misc-error
                                 #f
                                 "ccwl command ~S does not accept input key ~S. Accepted keys are ~S."
                                 (list (syntax->datum #'command)
                                       arg
                                       (command-input-keys command-object))
                                 #f))
                    (unless (memq value input-key-symbols)
                      (scm-error 'misc-error
                                 #f
                                 "ccwl step ~S supplied with unknown key ~S. Known keys at this step are ~S."
                                 (list step-id value input-key-symbols)
                                 #f))))
                 (syntax->datum (pairify #'(args ...))))
       (values (map (lambda (output)
                      (key (output-id output) step-id))
                    (command-outputs command-object))
               (list (make-step step-id
                                command-object
                                (map (match-lambda
                                       ((arg . value)
                                        (cons (keyword->symbol arg)
                                              (cwl-key-address
                                               (find (lambda (key)
                                                       (eq? value (key-name key)))
                                                     input-keys)))))
                                     (pairify (syntax->datum #'(args ...))))
                                (command-outputs command-object))))))
    ;; commands with an implicit step identifier
    ((command args ...)
     (workflow-steps #'(command (command) args ...)
                     input-keys))
    ;; any other unrecognized syntax
    (x (error "Unrecognized syntax:" (syntax->datum #'x)))))

(define (key->output key steps)
  "Return the <output> object corresponding to KEY, a <key> object, in
STEPS, a list of <step> objects. If no such <output> object is found,
return #f."
  (and-let* ((step-with-output (find (lambda (step)
                                       (eq? (step-id step)
                                            (key-step key)))
                                     steps))
             (output (find (lambda (output)
                             (eq? (output-id output)
                                  (key-name key)))
                           (step-out step-with-output))))
    (set-output-source output (cwl-key-address key))))

(define-syntax workflow
  (lambda (x)
    (syntax-case x ()
      ((_ inputs tree)
       (let* ((inputs (map (match-lambda
                             ((id args ...)
                              (apply input id args))
                             (id (input id)))
                           (syntax->datum #'inputs)))
              (output-keys steps (workflow-steps #'tree
                                                 (map (compose key input-id) inputs))))
         ;; TODO: Error out on duplicated step IDs.
         #`'#,(datum->syntax
               x
               (make-workflow steps
                              inputs
                              ;; Find the output object for each
                              ;; output key. Filter out global
                              ;; workflow inputs.
                              (filter-map (cut key->output <> steps)
                                          output-keys)))))
      (x (error "Unrecognized workflow syntax [expected (workflow (input ...) tree)]:"
                (syntax->datum #'x))))))
