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
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ccwl utils)
  #:use-module (ccwl yaml)
  #:export (command?
            command
            command-inputs
            command-outputs
            command-args
            command-stdin
            command-other
            workflow
            workflow?
            workflow-steps
            workflow-inputs
            workflow-outputs
            workflow-other
            input?
            input-id
            input-type
            input-label
            input-default
            input-position
            input-prefix
            input-other
            output?
            output-id
            output-type
            output-binding
            output-source
            output-other
            step?
            step-id
            step-run
            step-in
            step-out
            unspecified-default?))

(define-immutable-record-type <input>
  (make-input id type label default position prefix other)
  input?
  (id input-id)
  (type input-type)
  (label input-label)
  (default input-default)
  (position input-position set-input-position)
  (prefix input-prefix set-input-prefix)
  (other input-other))

(define-immutable-record-type <unspecified-default>
  (make-unspecified-default)
  unspecified-default?)

(define (input input-spec)
  "Return syntax to build an <input> object from INPUT-SPEC."
  (syntax-case input-spec ()
    ((id args ...) (identifier? #'id)
     (apply (syntax-lambda** (id #:key (type #'File) label (default (make-unspecified-default)) #:key* other)
              (let ((position #f)
                    (prefix #f))
                #`(make-input '#,id '#,type #,label
                              #,(if (unspecified-default? default)
                                    #'(make-unspecified-default)
                                    default)
                              #,position #,prefix '#,other)))
            #'(id args ...)))
    (id (identifier? #'id) (input #'(id)))
    (_ (error "Invalid input:" (syntax->datum input-spec)))))

(define-immutable-record-type <output>
  (make-output id type binding source other)
  output?
  (id output-id)
  (type output-type)
  (binding output-binding)
  (source output-source set-output-source)
  (other output-other))

(define (output output-spec)
  "Return syntax to build an <output> object from OUTPUT-SPEC."
  (syntax-case output-spec ()
    ((id args ...) (identifier? #'id)
     (apply (syntax-lambda** (id #:key (type #'File) binding source #:key* other)
              #`(make-output '#,id '#,type #,binding #,source '#,other))
            #'(id args ...)))
    (id (identifier? #'id) (output #'(id)))
    (_ (error "Invalid output:" (syntax->datum output-spec)))))

(define-immutable-record-type <step>
  (make-step id run in out)
  step?
  (id step-id)
  (run step-run)
  (in step-in)
  (out step-out))

(define-immutable-record-type <command>
  (make-command inputs outputs args stdin other)
  command?
  (inputs command-inputs)
  (outputs command-outputs)
  (args command-args)
  (stdin command-stdin)
  (other command-other))

(define-immutable-record-type <workflow>
  (make-workflow steps inputs outputs other)
  workflow?
  (steps workflow-steps)
  (inputs workflow-inputs)
  (outputs workflow-outputs)
  (other workflow-other))

(define (input-spec-id input-spec)
  "Return the identifier symbol of INPUT-SPEC."
  (syntax->datum
   (syntax-case input-spec ()
     ((id _ ...) (identifier? #'id) #'id)
     (id (identifier? #'id) #'id)
     (_ (error "Invalid input:" (syntax->datum input-spec))))))

(define (run-arg-position input-id run-args)
  "Return the position of input identified by symbol INPUT-ID in
RUN-ARGS. If such an input is not present in RUN-ARGS, return #f."
  (list-index (lambda (run-arg)
                (let ((run-arg-input
                       (syntax-case run-arg ()
                         (input (identifier? #'input)
                          (syntax->datum #'input))
                         ((_ input) (identifier? #'input)
                          (syntax->datum #'input))
                         (_ #f))))
                  (and run-arg-input
                       (eq? run-arg-input input-id))))
              run-args))

(define (run-arg-prefix input-id run-args)
  "Return the prefix of input identified by symbol INPUT-ID in
RUN-ARGS. If such an input is not present in RUN-ARGS, return #f."
  (any (lambda (x)
         (syntax-case x ()
           ((prefix input) (identifier? #'input)
            (and (eq? (syntax->datum #'input)
                      input-id)
                 #'prefix))
           (_ #f)))
       run-args))

;; TODO: Add fine-grained syntax checking.
(define-syntax command
  (lambda (x)
    (syntax-case x ()
      ((_ args ...)
       (apply (syntax-lambda** (#:key stdin #:key* inputs outputs run other)
                (unless run
                  (error "#:run key required in command definition" (syntax->datum x)))
                #`(make-command
                   (list #,@(map (lambda (input-spec)
                                   (let ((id (input-spec-id input-spec)))
                                     #`(set-input-prefix
                                        (set-input-position #,(input input-spec)
                                                            #,(run-arg-position id run))
                                        #,(run-arg-prefix id run))))
                                 inputs))
                   (list #,@(map output outputs))
                   (list #,@(map (lambda (x)
                                   (syntax-case x ()
                                     ;; Replace input symbol with quoted symbol.
                                     (input (identifier? #'input)
                                      #''input)
                                     ;; Leave string as is.
                                     (string-arg (string? (syntax->datum #'string-arg))
                                      #'string-arg)
                                     ;; Replace prefixed input symbol with
                                     ;; quoted symbol.
                                     ((prefix input) (and (string? (syntax->datum #'prefix))
                                                          (identifier? #'input))
                                      #''input)
                                     (_ (error "Invalid command element:"
                                               (syntax->datum x)))))
                                 run))
                   #,(and stdin #`'#,stdin)
                   (list #,@other)))
              #'(args ...))))))

(define (input=? input1 input2)
  (eq? (input-id input1)
       (input-id input2)))

(define (command-input-keys command)
  "Return the list of input keys accepted by COMMAND."
  (map input-id (command-inputs command)))

(define-immutable-record-type <key>
  (make-key name cwl-id step)
  key?
  (name key-name)
  (cwl-id key-cwl-id)
  (step key-step))

(define* (key name #:optional step (cwl-id name))
  "Build and return a <key> object."
  (make-key name cwl-id step))

;; TODO: Add docstring.
(define (cwl-key-address key)
  (if (key-step key)
      ;; Input/output of particular step
      (string-append (symbol->string (key-step key))
                     "/" (symbol->string (key-cwl-id key)))
      ;; Global input/output
      (symbol->string (key-cwl-id key))))

(define (command-object command-syntax)
  "Return the command object described by COMMAND-SYNTAX. If such a
command is not defined, return #f."
  (let ((var (module-variable (current-module)
                              (syntax->datum command-syntax))))
    (and var
         (command? (variable-ref var))
         (variable-ref var))))

(define (collect-steps x input-keys)
  "Traverse ccwl workflow body X and return two values---a list of
output keys and a list of steps. INPUT-KEYS is a list of supplied
input keys. Keys are represented by <key> objects, and steps are
represented by <step> objects."
  (syntax-case x (pipe tee)
    ;; pipe
    ((pipe expressions ...)
     (foldn (lambda (expression input-keys steps)
              (let ((input-keys child-steps (collect-steps expression input-keys)))
                (values input-keys (append steps child-steps))))
            #'(expressions ...)
            input-keys
            (list)))
    ;; tee
    ((tee expressions ...)
     (append-mapn (cut collect-steps <> input-keys)
                  #'(expressions ...)))
    ;; commands with only a single input and when only a single key is
    ;; available at this step
    ((command (step-id))
     (and (command-object #'command)
          (= (length input-keys) 1)
          (= (length (command-input-keys
                      (command-object #'command)))
             1))
     (collect-steps #`(command (step-id)
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
                     "Step `~S' missing required parameters `~S'"
                     (list step-id missing-parameters)
                     #f)))
       ;; Test for unknown keys.
       (for-each (match-lambda
                   ((arg . value)
                    (unless (memq (keyword->symbol arg)
                                  (command-input-keys command-object))
                      (scm-error 'misc-error
                                 #f
                                 "ccwl command `~S' does not accept input key `~S'. Accepted keys are `~S'."
                                 (list (syntax->datum #'command)
                                       arg
                                       (command-input-keys command-object))
                                 #f))
                    (unless (memq value input-key-symbols)
                      (scm-error 'misc-error
                                 #f
                                 "ccwl step `~S' supplied with unknown key `~S'. Known keys at this step are `~S'."
                                 (list step-id value input-key-symbols)
                                 #f))))
                 (syntax->datum (pairify #'(args ...))))
       (values (append (remove key-step input-keys)
                       (map (lambda (output)
                              (key (output-id output) step-id))
                            (command-outputs command-object)))
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
     (collect-steps #'(command (command) args ...)
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
                                  (key-cwl-id key)))
                           (step-out step-with-output))))
    (set-output-source output (cwl-key-address key))))

(define-syntax workflow
  (lambda (x)
    (syntax-case x ()
      ((_ (inputs ...) tree)
       #`(let ((input-objects (list #,@(map input #'(inputs ...))))
               (output-keys steps (collect-steps #'tree (map (compose key input-spec-id)
                                                             #'(inputs ...)))))
           ;; TODO: Error out on duplicated step IDs.
           ;; TODO: Implement escape hatch #:other in workflow syntax.
           (make-workflow steps
                          input-objects
                          ;; Find the output object for each
                          ;; output key. Filter out global
                          ;; workflow inputs.
                          (filter-map (cut key->output <> steps)
                                      output-keys)
                          '())))
      (x (error "Unrecognized workflow syntax [expected (workflow (input ...) tree)]:"
                (syntax->datum #'x))))))
