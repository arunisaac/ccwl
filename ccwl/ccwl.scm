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
  (source output-source)
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

(define* (command id arguments #:key (additional-inputs '()) (outputs '()) (other '()))
  (make-step id
             (make-command additional-inputs outputs arguments #f other)
             ;; A command can use the same input multiple times. So,
             ;; deduplicate.
             (delete-duplicates
              (append (filter input? arguments)
                      additional-inputs)
              input=?)
             outputs))

(define (input=? input1 input2)
  (string=? (input-id input1)
            (input-id input2)))

(define (auto-connect steps)
  "Auto-connect STEPS by matching inputs to outputs using their unique
identifiers. If any inputs are already matched, they are not
re-matched."
  (map (lambda (step)
         (set-step-in step
                      (map (lambda (input)
                             ;; If input is already connected, return
                             ;; it unaltered. Else, try to connect it
                             ;; to a source.
                             (cond
                              ((input-source input)
                               input)
                              ;; Input that should be connected to
                              ;; some intermediate output
                              ((find (lambda (step)
                                       (member (input-id input)
                                               (map output-id (step-out step))))
                                     steps)
                               => (lambda (source-step)
                                    (set-input-source input
                                                      (string-append (step-id source-step)
                                                                     "/" (input-id input)))))
                              ;; Non-internal input that should be
                              ;; interfaced with the outside world
                              (else input)))
                           (step-in step))))
       steps))

(define* (workflow id steps outputs #:key (other '()))
  "Build a Workflow class CWL workflow."
  (let* ((steps (auto-connect steps))
         (inputs
          ;; When the same input is used by multiple steps, there will
          ;; be duplicates. So, deduplicate.
          (delete-duplicates
           (append (append-map step-in steps)
                   ;; If an input is directly copied to the output, an
                   ;; output-source will be an <input> object.
                   (filter-map (lambda (output)
                                 (and (input? (output-source output))
                                      (output-source output)))
                               outputs))
           input=?))
         ;; List of non-internal inputs that should be interfaced with
         ;; the outside world.
         (interface-inputs (remove input-source inputs)))
    (make-step id
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
                                 interface-inputs))
                 (outputs . ,(map (lambda (output)
                                    `(,(output-id output)
                                      (type . ,(output-type output))
                                      (output-source . ,(match (output-source output)
                                                          ((? string? source) source)
                                                          ((? input? input) (input-id input))))))
                                  outputs))
                 (steps . ,(map (lambda (step)
                                  `(,(step-id step)
                                    (in . ,(map (lambda (input)
                                                  (cons (input-id input)
                                                        (or (input-source input)
                                                            (input-id input))))
                                                (step-in step)))
                                    (out . ,(list->vector (map output-id (step-out step))))
                                    (run . ,(match (step-run step)
                                              ((? command? command)
                                               (command->cwl command))
                                              (tree tree)))))
                                steps)))
               interface-inputs
               outputs)))

(define* (pipeline id steps
                   #:optional
                   (outputs
                    (list (output (string-append id "_stdout")
                                  #:source (string-append (step-id (last steps))
                                                          "/stdout")))))
  ;; Error out if any step does not encapsulate a command.
  (cond
   ((find (lambda (step)
            (not (command? (step-run step))))
          steps)
    => (lambda (step)
         (error "Step does not encapsulate command" step))))
  (workflow id
            (reverse
             (fold (lambda (step result)
                     (match result
                       ((previous-step tail ...)
                        (cons*
                         ;; Add an stdin input that is connected to the stdout
                         ;; of the previous step.
                         (let ((stdin (set-input-source %stdin
                                        (string-append (step-id previous-step) "/" (output-id %stdout)))))
                           (append-step-in (modify-step-run step
                                                            (cut set-command-stdin <> stdin))
                                           stdin))
                         previous-step
                         tail))
                       (() (list step))))
                   (list)
                   ;; Add an stdout output to all steps.
                   (map (lambda (step)
                          (append-step-out (modify-step-run step (cut append-command-outputs <> %stdout))
                                           %stdout))
                        steps)))
            outputs))

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
