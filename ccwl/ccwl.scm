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
  #:use-module (ice-9 match)
  #:use-module (ccwl utils)
  #:use-module (ccwl yaml)
  #:export (command
            workflow
            input
            input-with-prefix
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
  (prefix input-prefix set-input-prefix)
  (other input-other))

(define (input-with-prefix prefix input)
  "Set PREFIX on INPUT object."
  (set-input-prefix input prefix))

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

(define %stdin
  (input "stdin" #:type 'File))

(define %stdout
  (output "stdout" #:type 'stdout))

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
  (run step-run set-step-run)
  (in step-in set-step-in)
  (out step-out set-step-out))

(define (field-appender getter setter)
  (lambda (object element)
    (setter object (cons element (getter object)))))

(define (modify-step-run step proc)
  (set-step-run step (proc (step-run step))))

(define append-step-in
  (field-appender step-in set-step-in))

(define append-step-out
  (field-appender step-out set-step-out))

(define-immutable-record-type <command>
  (make-command additional-inputs outputs args stdin other)
  command?
  (additional-inputs command-additional-inputs)
  (outputs command-outputs set-command-outputs)
  (args command-args)
  (stdin command-stdin set-command-stdin)
  (other command-other))

(define append-command-outputs
  (field-appender command-outputs set-command-outputs))

(define command
  (lambda** (#:key* run (additional-inputs '()) (outputs '()) stdin (other '()))
    (make-command additional-inputs outputs run stdin other)))

(define (input=? input1 input2)
  (string=? (input-id input1)
            (input-id input2)))

(define (invoke-command step-id command . args)
  (make-step step-id
             command
             (plist->alist args)
             (command-outputs command)))

(define* (make-workflow steps outputs #:key (other '()))
  "Build a Workflow class CWL workflow."
  `((cwl-version . ,%cwl-version)
    (class . Workflow)
    (requirements (Subworkflow-feature-requirement))
    ,@other
    (inputs . ,(map (lambda (input)
                      `(,(input-id input)
                        ,@(filter-alist
                           `((type . ,(input-type input))
                             (label . ,(input-label input))
                             (default . ,(and (not (unspecified-default? (input-default input)))
                                              (input-default input)))))
                        ,@(input-other input)))
                    ;; When the same input is used by multiple steps,
                    ;; there will be duplicates. So, deduplicate.
                    (delete-duplicates
                     (append-map (lambda (step)
                                   (filter-map (match-lambda
                                                 ((_ . (? input? input))
                                                  input)
                                                 (_ #f))
                                               (step-in step)))
                                 steps)
                     input=?)))
    (outputs . ,(map (lambda (output)
                       `(,(output-id output)
                         (type . ,(match (output-type output)
                                    ('stdout 'File)
                                    (some-other-type some-other-type)))
                         (output-source . ,(match (output-source output)
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
                         (cons 'output-binding (output-binding output)))))
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
                     (string-append "$(inputs." (input-id arg) ")")
                     arg)
                 position))
              (command-args command)
              (iota (length (command-args command))))))
    `((cwl-version . ,%cwl-version)
      (class . Command-line-tool)
      ,@(command-other command)
      (arguments . ,(list->vector
                     ;; Put string arguments into the arguments array.
                     (filter-map (lambda (element)
                                   (and (string? (cli-element-argument element))
                                        `((position . ,(cli-element-position element))
                                          (value-from . ,(cli-element-argument element)))))
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
                                              (input-binding . ,(filter-alist
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
                                       (input-id (command-stdin command))
                                       ".path)")))
            '()))))

(define (write-cwl step file)
  (call-with-output-file file
    (cut scm->yaml (let ((run (step-run step)))
                     (if (command? run)
                         (command->cwl run)
                         run))
         <>)))

(define (workflow-steps x)
  (syntax-case x ()
    ((command (step-id) args ...)
     (cons #`(invoke-command
              step-id command
              #,@(append-map (lambda (pair)
                               (syntax-case pair ()
                                 ((key . (_ (id) _ ...))
                                  (list #'key (string-append
                                               (syntax->datum #'id)
                                               "/" (symbol->string
                                                    (keyword->symbol
                                                     (syntax->datum #'key))))))
                                 ((key . atom)
                                  (list #'key #'atom))))
                             (pairify #'(args ...))))
           (append-map workflow-steps #'(args ...))))
    (atom (list))))

(define-syntax workflow
  (lambda (x)
    (syntax-case x ()
      ((_ root)
       #`(make-workflow
          (list #,@(workflow-steps #'root))
          #,(syntax-case x ()
              ((_ (command (step-id) _ ...))
               #'(map (lambda (output)
                        (set-output-source
                         output (string-append step-id "/" (output-id output))))
                      (command-outputs command)))))))))
