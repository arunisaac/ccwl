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

;; This file implements conversion from ccwl objects (<workflow>,
;; <command>, <input>, <output>, <step>) to CWL YAML.

;;; Code:

(define-module (ccwl cwl)
  #:use-module (ice-9 match)
  #:use-module (ccwl ccwl)
  #:use-module (ccwl utils)
  #:use-module (ccwl yaml)
  #:export (workflow->cwl))

(define %cwl-version "v1.2")

(define (workflow->cwl workflow port)
  "Render WORKFLOW, a <workflow> object, to PORT as a CWL YAML
specification."
  (scm->yaml (workflow->cwl-scm workflow)
             port))

(define (filter-alist alist)
  "Filter ALIST removing entries with #f as the value. If the
resulting association list is empty, return #f. Else, return that
association list."
  (match (filter (match-lambda
                   ((_ . #f) #f)
                   (_ #t))
                 alist)
    (() #f)
    (result result)))

(define* (workflow->cwl-scm workflow)
  "Render WORKFLOW, a <workflow> object, into a CWL tree."
  `((cwlVersion . ,%cwl-version)
    (class . Workflow)
    (requirements (SubworkflowFeatureRequirement))
    ,@(workflow-other workflow)
    (inputs . ,(map (lambda (input)
                      `(,(input-id input)
                        ,@(filter-alist
                           `((type . ,(input-type input))
                             (label . ,(input-label input))
                             (default . ,(and (not (unspecified-default? (input-default input)))
                                              (input-default input)))))
                        ,@(input-other input)))
                    (workflow-inputs workflow)))
    (outputs . ,(map (lambda (output)
                       `(,(output-id output)
                         (type . ,(match (output-type output)
                                    ('stdout 'File)
                                    (some-other-type some-other-type)))
                         (outputSource . ,(match (output-source output)
                                            ((? string? source) source)
                                            ((? input? input) (input-id input))))))
                     (workflow-outputs workflow)))
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
                                  (command->cwl-scm command))
                                 (tree tree)))))
                   (workflow-steps workflow)))))

(define (output->cwl-scm output)
  "Render OUTPUT, a <output> object, into a CWL tree."
  `(,(output-id output)
    ,@(filter identity
              (list (and (output-type output)
                         (cons 'type (output-type output)))
                    (and (output-binding output)
                         (cons 'outputBinding (output-binding output)))))
    ,@(output-other output)))

(define (command->cwl-scm command)
  "Render COMMAND, a <command> object, into a CWL tree."
  `((cwlVersion . ,%cwl-version)
    (class . CommandLineTool)
    ,@(command-other command)
    (arguments . ,(list->vector
                   ;; Put string arguments into the arguments array.
                   (filter-mapi (lambda (arg index)
                                  (and (string? arg)
                                       `((position . ,index)
                                         (valueFrom . ,arg))))
                                (command-args command))))
    (inputs . ,(map (lambda (input)
                      `(,(input-id input)
                        ,@(filter-alist
                           `((type . ,(input-type input))
                             (label . ,(input-label input))
                             (default . ,(and (not (unspecified-default? (input-default input)))
                                              (input-default input)))
                             (inputBinding . ,(filter-alist
                                               `((position . ,(input-position input))
                                                 (prefix . ,(input-prefix input)))))))
                        ,@(input-other input)))
                    (command-inputs command)))
    (outputs . ,(map output->cwl-scm (command-outputs command)))
    ,@(if (command-stdin command)
          `((stdin . ,(string-append "$(inputs."
                                     (symbol->string
                                      (command-stdin command))
                                     ".path)")))
          '())))
