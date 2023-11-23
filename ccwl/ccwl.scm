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
  #:use-module (ccwl yaml)
  #:use-module (yaml)
  #:export (command?
            command
            command-inputs
            command-outputs
            command-args
            command-stdin
            command-stderr
            command-stdout
            command-requirements
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
            input-stage?
            input-other
            output?
            output-id
            output-type
            output-binding
            output-source
            output-other
            array-type?
            array-type-member-type
            step?
            step-id
            step-run
            step-in
            step-out
            step-scattered-inputs
            step-scatter-method
            unspecified-default?))

(define-immutable-record-type <input>
  (make-input id type label default position prefix stage? other)
  input?
  (id input-id)
  (type input-type)
  (label input-label)
  (default input-default set-input-default)
  (position input-position set-input-position)
  (prefix input-prefix set-input-prefix)
  (stage? input-stage?)
  (other input-other))

(define (memoize proc)
  "Return a memoized version of @var{proc}. Arguments to @var{proc} are
compared using @code{equal?}."
  (let ((memoized-results (list)))
    (lambda args
      (unless (assoc args memoized-results)
        (set! memoized-results
              (acons args (apply proc args)
                     memoized-results)))
      (assoc-ref memoized-results args))))

(define-immutable-record-type <array-type>
  (-make-array-type member-type)
  array-type?
  (member-type array-type-member-type))

;; We memoize the <array-type> constructor to enable easy comparison
;; using eq?.
(define make-array-type
  (memoize -make-array-type))

(define-immutable-record-type <unspecified-default>
  (make-unspecified-default)
  unspecified-default?)

(define (ensure-yaml-serializable tree parameter-name)
  "Raise an exception unless @var{tree} is serializable to YAML. Use
@var{parameter-name} in @code{&formatted-message} condition."
  ;; TODO: If tree is a quoted expression, emit a warning.
  (unless (false-if-exception
           (scm->yaml-string (syntax->datum tree)))
    (raise-exception
     (condition (ccwl-violation tree)
                (formatted-message (string-append parameter-name
                                                  " parameter not serializable to YAML"))))))

(define (construct-type-syntax type-spec)
  "Return syntax to build a type from @var{type-spec}."
  (syntax-case type-spec (array)
    ((array member-type)
     #`(make-array-type #,(construct-type-syntax #'member-type)))
    (primitive-type
     #''primitive-type)))

(define (type->syntax type)
  "Return syntax to recreate @var{type}."
  (if (array-type? type)
      #`(make-array-type #,(type->syntax (array-type-member-type type)))
      (with-syntax ((type (datum->syntax #f type)))
        #''type)))

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
       (apply (syntax-lambda** (id #:key (type #'File) label (default (make-unspecified-default)) (stage? #'#f) (other #'()))
                (unless (memq (syntax->datum stage?)
                              (list #t #f))
                  (raise-exception
                   (condition (ccwl-violation stage?)
                              (formatted-message "Invalid #:stage? parameter ~a. #:stage? must either be #t or #f."
                                                 (syntax->datum stage?)))))
                (ensure-yaml-serializable other "#:other")
                (let ((position #f)
                      (prefix #f))
                  #`(make-input '#,id
                                #,(construct-type-syntax type)
                                #,label
                                #,(if (unspecified-default? default)
                                      #'(make-unspecified-default)
                                      default)
                                #,position #,prefix #,stage? '#,other)))
              #'(id args ...))))
    (id (identifier? #'id) (input #'(id)))
    (_ (error "Invalid input:" (syntax->datum input-spec)))))

(define-immutable-record-type <output>
  (make-output id type binding source other)
  output?
  (id output-id set-output-id)
  (type output-type set-output-type)
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
       (apply (syntax-lambda** (id #:key (type #'File) binding source (other #'()))
                (ensure-yaml-serializable binding "#:binding")
                (ensure-yaml-serializable other "#:other")
                #`(make-output '#,id
                               #,(construct-type-syntax type)
                               '#,binding #,source '#,other))
              #'(id args ...))))
    (id (identifier? #'id) (output #'(id)))
    (_ (error "Invalid output:" (syntax->datum output-spec)))))

(define-immutable-record-type <command>
  (make-command inputs outputs args stdin stderr stdout requirements other)
  command?
  (inputs command-inputs set-command-inputs)
  (outputs command-outputs)
  (args command-args)
  (stdin command-stdin)
  (stderr command-stderr)
  (stdout command-stdout)
  (requirements command-requirements)
  (other command-other))

(define-immutable-record-type <cwl-workflow>
  (make-cwl-workflow file inputs outputs)
  cwl-workflow?
  (file cwl-workflow-file)
  (inputs cwl-workflow-inputs)
  (outputs cwl-workflow-outputs))

(define-immutable-record-type <workflow>
  (make-workflow steps inputs outputs other)
  workflow?
  (steps workflow-steps)
  (inputs workflow-inputs)
  (outputs workflow-outputs)
  (other workflow-other))

(define (function-outputs function)
  "Return the outputs of FUNCTION---a <command>, <cwl-workflow> or
<workflow> object."
  ((cond
    ((command? function) command-outputs)
    ((cwl-workflow? function) cwl-workflow-outputs)
    ((workflow? function) workflow-outputs)
    (else (error "Unrecognized ccwl function" function)))
   function))

(define-immutable-record-type <step>
  (-make-step id run in scattered-inputs scatter-method)
  step?
  (id step-id)
  (run step-run)
  (in step-in)
  (scattered-inputs step-scattered-inputs set-step-scattered-inputs)
  (scatter-method step-scatter-method set-step-scatter-method))

(define* (make-step id run in #:key (scattered-inputs '()) (scatter-method 'dotproduct))
  (-make-step id run in scattered-inputs scatter-method))

(define step-out (compose function-outputs step-run))

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

(define (run-args run defined-input-identifiers)
  "Return a list of run arguments specified in @var{run}
syntax. @var{defined-input-identifiers} is the list of input
identifiers defined in the commands."
  (define (syntax->run-arg x)
    (syntax-case x ()
      ;; Replace input symbol with quoted symbol.
      (input (identifier? #'input)
             ;; Ensure that specified input is defined in #:inputs of
             ;; command definition.
             (begin
               (unless (memq (syntax->datum #'input)
                             defined-input-identifiers)
                 (raise-exception
                  (condition (ccwl-violation #'input)
                             (formatted-message "Undefined input ~a"
                                                (syntax->datum #'input)))))
               (list #''input)))
      ;; Leave string as is.
      (string-arg (string? (syntax->datum #'string-arg))
                  (list #'string-arg))
      ;; Flatten prefixed string arguments. They have no
      ;; special meaning.
      ((prefix string-arg) (and (string? (syntax->datum #'prefix))
                                (string? (syntax->datum #'string-arg)))
       (list #'prefix #'string-arg))
      ;; Recurse on prefixed inputs.
      ((prefix input) (string? (syntax->datum #'prefix))
       (syntax->run-arg #'input))
      ;; Prefixes that are not strings
      ((prefix _)
       (raise-exception
        (condition (ccwl-violation #'prefix)
                   (formatted-message "Invalid prefix ~a. Prefixes must be strings."
                                      (syntax->datum #'prefix)))))
      (_
       (raise-exception
        (condition (ccwl-violation x)
                   (formatted-message "Invalid command element ~a. Command elements must either be input identifiers or literal strings."
                                      (syntax->datum x)))))))

  (append-map syntax->run-arg
              run))

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
         (apply (syntax-lambda** (#:key stdin stderr stdout (requirements #''()) (other #'()) #:key* inputs outputs run)
                  (when (null? run)
                    (raise-exception
                     (condition (ccwl-violation x)
                                (formatted-message "Missing ~a key in command definition"
                                                   #:run))))
                  (ensure-yaml-serializable other "#:other")
                  #`(make-command
                     (list #,@(map (lambda (input-spec)
                                     (let ((id (input-spec-id input-spec)))
                                       #`(set-input-prefix
                                          (set-input-position #,(input input-spec)
                                                              #,(run-arg-position id run))
                                          #,(run-arg-prefix id run))))
                                   inputs))
                     (list #,@(map output outputs))
                     (list #,@(run-args run (map input-spec-id inputs)))
                     #,(and stdin #`'#,stdin)
                     #,(if (and stderr
                                (not (string? (syntax->datum stderr))))
                           (raise-exception
                            (condition (ccwl-violation stderr)
                                       (formatted-message "Invalid #:stderr parameter ~a. #:stderr parameter must be a string"
                                                          (syntax->datum stderr))))
                           stderr)
                     #,(if (and stdout
                                (not (string? (syntax->datum stdout))))
                           (raise-exception
                            (condition (ccwl-violation stdout)
                                       (formatted-message "Invalid #:stdout parameter ~a. #:stdout parameter must be a string"
                                                          (syntax->datum stdout))))
                           stdout)
                     #,requirements
                     '#,other))
                #'(args ...)))))))

(define-syntax cwl-workflow
  (lambda (x)
    (syntax-case x ()
      ((_ file-syntax)
       (let ((file (syntax->datum #'file-syntax))
             (parameters->id+type
              (lambda (parameters)
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
                         parameters)))))
         (unless (file-exists? file)
           (raise-exception
            (condition (ccwl-violation #'file-syntax)
                       (formatted-message "CWL workflow file ~a does not exist" file))))
         ;; Read inputs/outputs from CWL workflow YAML file and build
         ;; a <cwl-workflow> object.
         (let ((yaml (read-yaml-file file)))
           #`(make-cwl-workflow
              file-syntax
              (list #,@(map (match-lambda
                             ((id . type)
                              (with-syntax ((id (datum->syntax #f id))
                                            (type (datum->syntax #f type)))
                                #`(make-input 'id 'type #f #f #f #f #f '()))))
                           (parameters->id+type (assoc-ref yaml "inputs"))))
              (list #,@(map (match-lambda
                             ((id . type)
                              (with-syntax ((id (datum->syntax #f id))
                                            (type (datum->syntax #f type)))
                                #`(make-output 'id 'type '() #f '()))))
                           (parameters->id+type (assoc-ref yaml "outputs")))))))))))

(define (function-inputs function)
  "Return the list of inputs accepted by @var{function}---a
@code{<command>}, @code{<cwl-workflow>} or @code{<workflow> object."
  ((cond
    ((command? function) command-inputs)
    ((cwl-workflow? function) cwl-workflow-inputs)
    ((workflow? function) workflow-inputs)
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
  "Return the ccwl function object (a <command>, <cwl-workflow>
or <workflow> object) described by syntax X. If such a ccwl function
is not defined, return #f."
  ;; TODO: What if function object is defined in lexical scope?
  (let ((result (false-if-exception
                 (eval (syntax->datum x)
                       (interaction-environment)))))
    (and (or (command? result)
             (cwl-workflow? result)
             (workflow? result))
         result)))

(define (collect-scatter-step x input-keys scatter-method)
  "Return a list of output keys and a list of steps from scatter workflow
clause @var{x} and @var{scatter-method}. @var{input-keys} is a list of
supplied input keys."
  (syntax-case x ()
    ((_ (function-spec ...) scattered-args ...)
     (let ((keys steps
                 (collect-steps #'(function-spec ... scattered-args ...)
                                input-keys)))
       (values keys
               (map (lambda (step)
                      (set-step-scattered-inputs
                       (set-step-scatter-method step scatter-method)
                       (map (match-lambda
                              ((key . _) (keyword->symbol key)))
                            (syntax->datum (pairify #'(scattered-args ...))))))
                    steps))))))

(define (collect-steps x input-keys)
  "Traverse ccwl workflow body X and return two values---a list of
output keys and a list of steps. INPUT-KEYS is a list of supplied
input keys. Keys are represented by <key> objects, and steps are
represented by <step> objects."
  (syntax-case x (pipe tee rename scatter scatter-cross scatter-nested-cross)
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
    ;; rename keys (base case)
    ((rename new-key old-key)
     (begin
       ;; Error out on non-keyword arguments.
       (unless (keyword? (syntax->datum #'new-key))
         (raise-exception
          (condition (ccwl-violation #'new-key)
                     (formatted-message "Expected keyword (for example: #:foo, #:bar)"))))
       ;; Ensure old key exists.
       (unless (memq (syntax->datum #'old-key)
                     (map key-name input-keys))
         (raise-exception
          (condition (ccwl-violation #'old-key)
                     (formatted-message "Unknown key ~a. Known keys at this step are ~a."
                                        (syntax->datum #'old-key)
                                        (map key-name input-keys)))))
       (values (map (lambda (key)
                      ;; Rename one key. Pass the rest through.
                      (or (and (eq? (syntax->datum #'old-key)
                                    (key-name key))
                               (set-key-name key (keyword->symbol (syntax->datum #'new-key))))
                          key))
                    input-keys)
               (list))))
    ;; rename keys (recurse)
    ((rename new-key old-key other-mappings ...)
     (collect-steps #'(pipe (rename new-key old-key)
                            (rename other-mappings ...))
                    input-keys))
    ;; TODO: Support cross product scatter methods.
    ((scatter _ ...)
     (collect-scatter-step x input-keys 'dotproduct))
    ((scatter-cross _ ...)
     (collect-scatter-step x input-keys 'flat_crossproduct))
    ((scatter-nested-cross _ ...)
     (collect-scatter-step x input-keys 'nested_crossproduct))
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
                    (when (and (symbol? (syntax->datum value))
                               (not (memq (syntax->datum value)
                                          input-key-symbols)))
                      (raise-exception
                       (condition (ccwl-violation value)
                                  (formatted-message "Step ~a supplied with unknown key ~a. Known keys at this step are ~a."
                                                     step-id-symbol
                                                     (syntax->datum value)
                                                     input-key-symbols))))))
                 (pairify #'(args ...)))
       ;; Test for arguments that have been supplied more than once.
       (fold (match-lambda*
               (((key-syntax . _) seen)
                (let ((key (syntax->datum key-syntax)))
                  (when (memq key seen)
                    (raise-exception
                     (condition (ccwl-violation key-syntax)
                                (formatted-message "~a argument already supplied"
                                                   key))))
                  (cons key seen))))
             (list)
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
            (values (append
                     ;; Pass global workflow inputs through. Thus,
                     ;; these are globally visible to all steps, and
                     ;; are a kind of "global variable".
                     (remove key-step input-keys)
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
<output> object is found, return #f. Note that the returned syntax is
only applicable to construct <output> objects for workflows, not in
commands."
  (define (stdout->file-type type)
    "Recursively convert stdout types in @var{type} to File types."
    (cond
     ((array-type? type)
      (make-array-type (stdout->file-type (array-type-member-type type))))
     ((eq? type 'stdout) 'File)
     (else type)))

  (and-let* ((step-with-output (find (lambda (step)
                                       (eq? (step-id step)
                                            (key-step key)))
                                     steps)))
    (let* ((output-for-key
            ;; Find output object that corresponds to key.
            (find (lambda (output)
                    (eq? (output-id output)
                         (key-cwl-id key)))
                  (function-outputs
                   (function-object
                    (step-run step-with-output)))))
           (output
            (set-output-type
             ;; Set output id and source fields from key.
             (set-output-id
              (set-output-source output-for-key
                                 (cwl-key-address key))
              (key-name key))
             ;; Convert stdout type outputs to File type outputs.
             (let ((type (stdout->file-type (output-type output-for-key))))
               ;; If step scatters, convert to an array type.
               (cond
                ((null? (step-scattered-inputs step-with-output))
                 type)
                ;; For dot products and flat cross products, create a
                ;; singly nested array type.
                ((memq (step-scatter-method step-with-output)
                       '(dotproduct flat_crossproduct))
                 (make-array-type type))
                ;; For nested cross products, create a sufficiently
                ;; nested array type.
                ((eq? (step-scatter-method step-with-output)
                      'nested_crossproduct)
                 (fold (lambda (_ result)
                         (make-array-type result))
                       type
                       (step-scattered-inputs step-with-output)))
                (else
                 (error "This should not be possible!")))))))
      ;; Construct syntax to recreate output object.
      #`(make-output
         #,(with-syntax ((id (datum->syntax #f (output-id output))))
             #''id)
         #,(type->syntax (output-type output))
         #,(with-syntax ((binding (datum->syntax #f (output-binding output))))
             #''binding)
         #,(output-source output)
         #,(with-syntax ((other (datum->syntax #f (output-other output))))
             #''other)))))

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
                                   #''in)
                               #:scattered-inputs #,(with-syntax ((scattered-inputs
                                                                   (datum->syntax #f (step-scattered-inputs step))))
                                                      #''scattered-inputs)
                               #:scatter-method #,(with-syntax ((scatter-method
                                                                 (datum->syntax #f (step-scatter-method step))))
                                                    #''scatter-method)))
                          steps))
            (list #,@(map input #'(inputs ...)))
            ;; Find the output object for each output
            ;; key. Filter out global workflow inputs.
            (list #,@(filter-map (cut key->output <> steps)
                                 output-keys))
            '())))
      ;; Guess that these are multiple unconnected expressions in the
      ;; workflow body, and try to produce a helpful error message.
      ((_ (inputs ...) expressions ...)
       (raise-exception
        (condition
         (ccwl-violation x)
         (formatted-message "More than one expression ~a in workflow body. Perhaps you need to combine them with a pipe or a tee?"
                            (string-join
                             (map (lambda (expression)
                                    (call-with-output-string
                                      (cut write expression <>)))
                                  (syntax->datum #'(expressions ...))))))))
      (x
       (raise-exception
        (condition (ccwl-violation #'x)
                   (formatted-message "Unrecognized workflow syntax [expected (workflow (input ...) tree)]")))))))
