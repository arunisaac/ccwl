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

;;; Commentary:

;; This is the main module that presents the public interface to ccwl.

;;; Code:

(define-module (ccwl ccwl)
  #:use-module ((rnrs conditions) #:select (condition
                                            condition-irritants))
  #:use-module ((rnrs exceptions) #:select (guard (raise . raise-exception)))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ccwl conditions)
  #:use-module (ccwl utils)
  #:use-module (yaml)
  #:export (command?
            command
            command-inputs
            command-outputs
            command-args
            command-stdin
            command-other
            cwl-workflow?
            cwl-workflow
            cwl-workflow-file
            cwl-workflow-inputs
            cwl-workflow-outputs
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
  (default input-default set-input-default)
  (position input-position set-input-position)
  (prefix input-prefix set-input-prefix)
  (other input-other))

(define-immutable-record-type <unspecified-default>
  (make-unspecified-default)
  unspecified-default?)

(define (input input-spec)
  "Return syntax to build an <input> object from INPUT-SPEC."
  (syntax-case input-spec ()
    ((id args ...)
     (guard (exception
             ((unrecognized-keyword-assertion? exception)
              (raise-exception
               (match (condition-irritants exception)
                 ((irritant _ ...)
                  (condition (ccwl-violation irritant)
                             (formatted-message "Unrecognized keyword argument ~a in input"
                                                (syntax->datum irritant)))))))
             ((invalid-keyword-arity-assertion? exception)
              (raise-exception
               (match (condition-irritants exception)
                 ;; TODO: Report all extra arguments, not just the
                 ;; first one.
                 ((keyword _ extra _ ...)
                  (condition (ccwl-violation extra)
                             (formatted-message "Unexpected extra argument ~a for unary keyword argument ~a"
                                                (syntax->datum extra)
                                                (syntax->datum keyword)))))))
             ((invalid-positional-arguments-arity-assertion? exception)
              (raise-exception
               (match (condition-irritants exception)
                 ;; TODO: Report all extra positional arguments, not
                 ;; just the first one.
                 ((id extra _ ...)
                  (condition (ccwl-violation extra)
                             (formatted-message "Unexpected extra positional argument ~a in input"
                                                (syntax->datum extra))))
                 (()
                  (condition (ccwl-violation input-spec)
                             (formatted-message "Input has no identifier")))))))
       (apply (syntax-lambda** (id #:key (type #'File) label (default (make-unspecified-default)) #:key* other)
                (let ((position #f)
                      (prefix #f))
                  #`(make-input '#,id '#,type #,label
                                #,(if (unspecified-default? default)
                                      #'(make-unspecified-default)
                                      default)
                                #,position #,prefix '#,other)))
              #'(id args ...))))
    (id (identifier? #'id) (input #'(id)))
    (_ (error "Invalid input:" (syntax->datum input-spec)))))

(define-immutable-record-type <output>
  (make-output id type binding source other)
  output?
  (id output-id set-output-id)
  (type output-type)
  (binding output-binding)
  (source output-source set-output-source)
  (other output-other))

(define (output output-spec)
  "Return syntax to build an <output> object from OUTPUT-SPEC."
  (syntax-case output-spec ()
    ((id args ...) (identifier? #'id)
     (guard (exception
             ((unrecognized-keyword-assertion? exception)
              (raise-exception
               (match (condition-irritants exception)
                 ((irritant _ ...)
                  (condition (ccwl-violation irritant)
                             (formatted-message "Unrecognized keyword argument ~a in output"
                                                (syntax->datum irritant)))))))
             ((invalid-keyword-arity-assertion? exception)
              (raise-exception
               (match (condition-irritants exception)
                 ;; TODO: Report all extra arguments, not just the
                 ;; first one.
                 ((keyword _ extra _ ...)
                  (condition (ccwl-violation extra)
                             (formatted-message "Unexpected extra argument ~a for unary keyword argument ~a"
                                                (syntax->datum extra)
                                                (syntax->datum keyword)))))))
             ((invalid-positional-arguments-arity-assertion? exception)
              (raise-exception
               (match (condition-irritants exception)
                 ;; TODO: Report all extra positional arguments, not
                 ;; just the first one.
                 ((id extra _ ...)
                  (condition (ccwl-violation extra)
                             (formatted-message "Unexpected extra positional argument ~a in output"
                                                (syntax->datum extra))))
                 (()
                  (condition (ccwl-violation output-spec)
                             (formatted-message "Output has no identifier")))))))
       (apply (syntax-lambda** (id #:key (type #'File) binding source #:key* other)
                #`(make-output '#,id '#,type #,binding #,source '#,other))
              #'(id args ...))))
    (id (identifier? #'id) (output #'(id)))
    (_ (error "Invalid output:" (syntax->datum output-spec)))))

(define-immutable-record-type <command>
  (make-command inputs outputs args stdin other)
  command?
  (inputs command-inputs set-command-inputs)
  (outputs command-outputs)
  (args command-args)
  (stdin command-stdin)
  (other command-other))

(define-immutable-record-type <cwl-workflow>
  (make-cwl-workflow file inputs outputs)
  cwl-workflow?
  (file cwl-workflow-file)
  (inputs cwl-workflow-inputs)
  (outputs cwl-workflow-outputs))

(define (function-outputs function)
  "Return the outputs of FUNCTION, a <command> or <cwl-workflow>
object."
  ((cond
    ((command? function) command-outputs)
    ((cwl-workflow? function) cwl-workflow-outputs)
    (else (error "Unrecognized ccwl function" function)))
   function))

(define-immutable-record-type <step>
  (make-step id run in)
  step?
  (id step-id)
  (run step-run)
  (in step-in))

(define step-out (compose function-outputs step-run))

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
     ((id _ ...)
      (if (not (identifier? #'id))
          (raise-exception
           (condition (ccwl-violation input-spec)
                      (formatted-message "Input has no identifier")))
          #'id))
     (id (identifier? #'id) #'id)
     (_ (raise-exception (condition (ccwl-violation input-spec)
                                    (formatted-message "Invalid input")))))))

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
       (guard (exception
               ((unrecognized-keyword-assertion? exception)
                (raise-exception
                 (match (condition-irritants exception)
                   ((irritant _ ...)
                    (condition (ccwl-violation irritant)
                               (formatted-message "Unrecognized keyword argument ~a in command definition"
                                                  (syntax->datum irritant)))))))
               ((invalid-keyword-arity-assertion? exception)
                (raise-exception
                 (match (condition-irritants exception)
                   ;; TODO: Report all extra arguments, not just the
                   ;; first one.
                   ((keyword _ extra _ ...)
                    (condition (ccwl-violation extra)
                               (formatted-message "Unexpected extra argument ~a for unary keyword argument ~a"
                                                  (syntax->datum extra)
                                                  (syntax->datum keyword)))))))
               ((invalid-positional-arguments-arity-assertion? exception)
                (raise-exception
                 (match (condition-irritants exception)
                   ;; TODO: Report all extra positional arguments, not
                   ;; just the first one.
                   ((extra _ ...)
                    (condition (ccwl-violation extra)
                               (formatted-message "Unexpected extra positional argument ~a in command definition"
                                                  (syntax->datum extra))))))))
         (apply (syntax-lambda** (#:key stdin #:key* inputs outputs run other)
                  (when (null? run)
                    (raise-exception
                     (condition (ccwl-violation x)
                                (formatted-message "Missing ~a key in command definition"
                                                   #:run))))
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
                #'(args ...)))))))

(define (cwl-workflow file)
  (define (parameters->id+type parameters)
    (if (vector? parameters)
        ;; Vector of dictionaries
        (map (lambda (alist)
               (cons (string->symbol (assoc-ref alist "id"))
                     (string->symbol (assoc-ref alist "type"))))
             (vector->list parameters))
        ;; One dictionary
        (map (match-lambda
               ((id . (? string? type))
                (cons (string->symbol id)
                      (string->symbol type)))
               ((id . alist)
                (cons (string->symbol id)
                      (string->symbol (assoc-ref alist "type")))))
             parameters)))

  (unless (file-exists? file)
    (error "CWL workflow file does not exist" file))
  ;; Read inputs/outputs from CWL workflow YAML file and build a
  ;; <cwl-workflow> object.
  (let ((yaml (read-yaml-file file)))
    (make-cwl-workflow file
                       (map (match-lambda
                              ((id . type)
                               (make-input id type #f #f #f #f #f)))
                            (parameters->id+type (assoc-ref yaml "inputs")))
                       (map (match-lambda
                              ((id . type)
                               (make-output id type #f #f #f)))
                            (parameters->id+type (assoc-ref yaml "outputs"))))))

(define (input=? input1 input2)
  (eq? (input-id input1)
       (input-id input2)))

(define (function-inputs function)
  "Return the list of inputs accepted by @var{function}, a
@code{<command>} or @code{<cwl-workflow>} object."
  ((cond
    ((command? function) command-inputs)
    ((cwl-workflow? function) cwl-workflow-inputs)
    (else (error "Unrecognized ccwl function" function)))
   function))

(define (function-input-keys function)
  "Return the list of input keys accepted by FUNCTION, a <command>
object or a <cwl-workflow> object."
  (map input-id
       (function-inputs function)))

(define-immutable-record-type <key>
  (make-key name cwl-id step)
  key?
  (name key-name set-key-name)
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

(define (apply-partially command partial-arguments)
  "Return a new command that is a partial application of
@var{partial-arguments} to @var{command}. @var{partial-arguments} is
an association list mapping keyword arguments to their values."
  (set-command-inputs command
    (map (lambda (input)
           (set-input-default input
             (or (any (match-lambda
                        ((arg . value)
                         (and (eq? (input-id input)
                                   (keyword->symbol arg))
                              value)))
                      partial-arguments)
                 (input-default input))))
         (command-inputs command))))

(define (function-object x)
  "Return the ccwl function object (a <command> or a <cwl-workflow>
object) described by syntax X. If such a ccwl function is not defined,
return #f."
  ;; TODO: What if function object is defined in lexical scope?
  (let ((result (false-if-exception
                 (eval (syntax->datum x)
                       (interaction-environment)))))
    (and (or (command? result)
             (cwl-workflow? result))
         result)))

(define (collect-steps x input-keys)
  "Traverse ccwl workflow body X and return two values---a list of
output keys and a list of steps. INPUT-KEYS is a list of supplied
input keys. Keys are represented by <key> objects, and steps are
represented by <step> objects."
  (syntax-case x (pipe tee rename)
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
    ;; rename keys
    ((rename mapping ...)
     (values (map (lambda (key)
                    (or (any (match-lambda
                               ((new . old)
                                (and (eq? old (key-name key))
                                     (set-key-name key (keyword->symbol new)))))
                             (syntax->datum (pairify #'(mapping ...))))
                        key))
                  input-keys)
             (list)))
    ((function (step-id) args ...)
     ;; Run a whole bunch of tests so that we can produce useful error
     ;; messages.
     (let ((input-key-symbols (map key-name input-keys))
           (function-object (function-object #'function))
           (step-id-symbol (syntax->datum #'step-id)))
       ;; Test for undefined command.
       (unless function-object
         (raise-exception
          (condition (ccwl-violation #'function)
                     (formatted-message "Undefined ccwl command ~a"
                                        (syntax->datum #'function)))))
       ;; Test for missing required parameters.
       (match (lset-difference
               eq?
               (filter-map (lambda (input)
                             ;; Leave out inputs that have default values.
                             (and (unspecified-default? (input-default input))
                                  (input-id input)))
                           (function-inputs function-object))
               (map (match-lambda
                      ((key . _) (keyword->symbol key)))
                    (syntax->datum (pairify #'(args ...)))))
         (() #t)
         (missing-parameters
          (raise-exception
           ;; TODO: Report entire form, not just the name of the
           ;; step.
           (condition (ccwl-violation #'function)
                      (formatted-message "Step ~a missing required parameters ~a"
                                         step-id-symbol
                                         (map symbol->keyword missing-parameters))))))
       ;; Test for unknown keys.
       (for-each (match-lambda
                   ((arg . value)
                    (unless (memq (keyword->symbol (syntax->datum arg))
                                  (function-input-keys function-object))
                      (raise-exception
                       ;; TODO: Report arg and value, not just arg.
                       (condition (ccwl-violation arg)
                                  ;; TODO: Do not report accepted keys
                                  ;; that have already been satisfied.
                                  (formatted-message "Step ~a does not accept input key ~a. Accepted keys are ~a."
                                                     step-id-symbol
                                                     (syntax->datum arg)
                                                     (map symbol->keyword
                                                          (function-input-keys function-object))))))
                    ;; If value is neither a literal nor a known key,
                    ;; error out.
                    (when (and (symbol? value)
                               (not (memq (syntax->datum value)
                                          input-key-symbols)))
                      (raise-exception
                       (condition (ccwl-violation value)
                                  (formatted-message "Step ~a supplied with unknown key ~a. Known keys at this step are ~a."
                                                     step-id-symbol
                                                     (syntax->datum value)
                                                     input-key-symbols))))))
                 (pairify #'(args ...)))
       (let ((symbolic-arguments
              literal-arguments
              (partition (match-lambda
                           ((_ . value)
                            (symbol? value)))
                         (pairify (syntax->datum #'(args ...))))))
         (match literal-arguments
           ;; If there are no literal arguments, construct <step>
           ;; object.
           (()
            (values (append (remove key-step input-keys)
                            (map (lambda (output)
                                   (key (output-id output) step-id-symbol))
                                 (function-outputs function-object)))
                    (list (make-step step-id-symbol
                                     #'function
                                     (map (match-lambda
                                            ((arg . value)
                                             (cons (keyword->symbol arg)
                                                   (cwl-key-address
                                                    (find (lambda (key)
                                                            (eq? value (key-name key)))
                                                          input-keys)))))
                                          (pairify (syntax->datum #'(args ...))))))))
           ;; If literal values are provided as arguments, partially
           ;; apply those literal values to the command and recurse.
           (_
            (collect-steps #`(((module-ref (resolve-module '(ccwl ccwl))
                                           'apply-partially)
                               function '#,literal-arguments)
                              (step-id)
                              #,@(append-map (match-lambda
                                               ((arg . value)
                                                (list arg value)))
                                             symbolic-arguments))
                           input-keys))))))
    ;; ccwl functions with an implicit step identifier
    ((function args ...)
     ;; Ensure that steps with expression commands have identifiers.
     (unless (symbol? (syntax->datum #'function))
       (raise-exception
        (condition (ccwl-violation #'function)
                   (formatted-message "Step with expression ~a that evaluates to a command must have identifier"
                                      (syntax->datum #'function)))))
     (collect-steps #'(function (function) args ...)
                    input-keys))
    ;; any other unrecognized syntax
    (x (error "Unrecognized syntax:" (syntax->datum #'x)))))

(define (key->output key steps)
  "Return syntax to construct an <output> object corresponding to KEY,
a <key> object, in STEPS, a list of <step> objects. If no such
<output> object is found, return #f."
  (and-let* ((step-with-output (find (lambda (step)
                                       (eq? (step-id step)
                                            (key-step key)))
                                     steps)))
    (with-syntax ((key-name (datum->syntax #f (key-name key)))
                  (key-cwl-id (datum->syntax #f (key-cwl-id key))))
      #`(set-output-id
         (set-output-source (find (lambda (output)
                                    (eq? (output-id output)
                                         'key-cwl-id))
                                  (function-outputs
                                   #,(step-run step-with-output)))
                            #,(cwl-key-address key))
         'key-name))))

(define-syntax workflow
  (lambda (x)
    (syntax-case x ()
      ((_ (inputs ...) tree)
       (let ((output-keys steps (collect-steps
                                 #'tree (map (compose key input-spec-id)
                                             #'(inputs ...)))))
         ;; TODO: Error out on duplicated step IDs.
         ;; TODO: Implement escape hatch #:other in workflow syntax.
         #`(make-workflow
            (list #,@(map (lambda (step)
                            #`(make-step
                               #,(with-syntax ((id (datum->syntax #f (step-id step))))
                                   #''id)
                               #,(step-run step)
                               #,(with-syntax ((in (datum->syntax #f (step-in step))))
                                   #''in)))
                          steps))
            (list #,@(map input #'(inputs ...)))
            ;; Find the output object for each output
            ;; key. Filter out global workflow inputs.
            (list #,@(filter-map (cut key->output <> steps)
                                 output-keys))
            '())))
      (x (error "Unrecognized workflow syntax [expected (workflow (input ...) tree)]:"
                (syntax->datum #'x))))))
