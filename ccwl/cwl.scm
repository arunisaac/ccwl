;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2023–2025 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ccwl ccwl)
  #:use-module (ccwl utils)
  #:use-module (ccwl yaml)
  #:export (workflow->cwl
            command->cwl
            function->cwl))

(define %cwl-version "v1.2")

(define function->cwl
  (match-lambda*
    (((? workflow? workflow) port)
     (workflow->cwl workflow port))
    (((? command? command) port)
     (command->cwl command port))
    (((? js-expression? expression) port)
     (js-expression->cwl expression port))))

(define (workflow->cwl workflow port)
  "Render WORKFLOW, a <workflow> object, to PORT as a CWL YAML
specification."
  (scm->yaml (workflow->cwl-scm workflow)
             port))

(define (vector-filter-map proc vec)
  "Map @var{proc} over @var{vec} and return a vector of the results that
are not @code{#f}."
  (list->vector (filter-map proc (vector->list vec))))

(define (prune-tree tree)
  "Prune JSON-like @var{tree} removing dictionary keys without a mapping."
  (match tree
    ;; Array
    (#(elements ...)
     (vector-filter-map (lambda (element)
                          (match (prune-tree element)
                            (() #f)
                            (pruned-element pruned-element)))
                        tree))
    ;; Dictionary
    ((pairs ...)
     (filter-map (match-lambda
                   ;; Key with a mapping
                   ((key . value)
                    (match (prune-tree value)
                      (() #f)
                      (pruned-value (cons key pruned-value))))
                   ;; Key without a mapping
                   ((key) #f))
                 pairs))
    ;; Atom
    (atom atom)))

(define* (workflow->cwl-scm workflow)
  "Render WORKFLOW, a <workflow> object, into a CWL tree."
  `((cwlVersion . ,%cwl-version)
    (class . Workflow)
    (requirements . ((SubworkflowFeatureRequirement)
                     ,@(if (every (compose null? step-scattered-inputs)
                                  (workflow-steps workflow))
                           '()
                           '((ScatterFeatureRequirement)))))
    ,@(workflow-other workflow)
    (inputs . ,(map input->cwl-scm
                    (workflow-inputs workflow)))
    (outputs . ,(map (cut output->cwl-scm <> #:workflow? #t)
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
                                 ((? js-expression? expression)
                                  (js-expression->cwl-scm expression))
                                 ((? cwl-workflow? cwl-workflow)
                                  (cwl-workflow-file cwl-workflow))
                                 ((? workflow? workflow)
                                  (workflow->cwl-scm workflow))
                                 (tree tree)))
                       ,@(match (step-scattered-inputs step)
                           (() '())
                           (scattered-inputs
                            `((scatter . ,(list->vector scattered-inputs))
                              (scatterMethod . ,(step-scatter-method step)))))))
                   (workflow-steps workflow)))))

(define (type->cwl type)
  "Render @var{type} into a CWL tree."
  (if (array-type? type)
      `((type . array)
        (items . ,(type->cwl (array-type-member-type type))))
      type))

(define* (output->cwl-scm output #:key workflow?)
  "Render @var{output}, a @code{<output>} object, into a CWL tree. If
@var{workflow?} is @code{#t}, this is a workflow output."
  `(,(output-id output)
    ,@(prune-tree
       `((type . ,(type->cwl (output-type output)))
         ;; outputBinding is relevant only to commands, and
         ;; outputSource is relevant only to workflows.
         ,@(if workflow?
               `((outputSource . ,(match (output-source output)
                                    ((? string? source) source)
                                    ((? input? input) (input-id input)))))
               `((outputBinding . ,(or (output-binding output)
                                       '()))))
         ,@(output-other output)))))

(define (command->cwl command port)
  "Render @var{command}, a @code{<command>} object, to @var{port} as a
CWL YAML specification."
  (scm->yaml (command->cwl-scm command)
             port))

(define (input->cwl-scm input)
  "Render @var{input}, a @code{<input>} object, into a CWL tree."
  `(,(input-id input)
    ,@(prune-tree
       `((type . ,(type->cwl (input-type input)))
         (default . ,(if (unspecified-default? (input-default input))
                         '()
                         (input-default input)))
         (label . ,(or (input-label input)
                       '()))
         ;; inputBinding is only relevant to commands, not workflows.
         ;; But, the input position and prefix are not set for worklow
         ;; inputs and therefore this sub-expression has no effect.
         ;; So, leave this be.
         (inputBinding
          (position . ,(or (input-position input)
                           '()))
          (prefix . ,(or (input-prefix input)
                         '()))
          (itemSeparator . ,(or (input-separator input)
                                '())))
         ,@(input-other input)))))

(define (staging-requirements inputs)
  "Return @samp{InitialWorkDirRequirement} to stage any @var{inputs} that
must be staged."
  (if (any input-stage? inputs)
      ;; Stage any inputs that need to be.
      `((InitialWorkDirRequirement
         (listing . ,(list->vector
                      (filter-map (lambda (input)
                                    (and (input-stage? input)
                                         (string-append "$(inputs."
                                                        (symbol->string (input-id input))
                                                        ")")))
                                  inputs)))))
      '()))

(define (command->cwl-scm command)
  "Render COMMAND, a <command> object, into a CWL tree."
  `((cwlVersion . ,%cwl-version)
    (class . CommandLineTool)
    (requirements
     ,@(staging-requirements (command-inputs command))
     ,@(command-requirements command))
    ,@(command-other command)
    (arguments . ,(list->vector
                   ;; Put string arguments into the arguments array.
                   (filter-mapi (lambda (arg index)
                                  (and (string? arg)
                                       `((position . ,index)
                                         (valueFrom . ,arg))))
                                (command-args command))))
    (inputs . ,(map input->cwl-scm
                    (command-inputs command)))
    (outputs . ,(map output->cwl-scm (command-outputs command)))
    ,@(if (command-stdin command)
          `((stdin . ,(string-append "$(inputs."
                                     (symbol->string
                                      (command-stdin command))
                                     ".path)")))
          '())
    ,@(if (command-stderr command)
          `((stderr . ,(command-stderr command)))
          '())
    ,@(if (command-stdout command)
          `((stdout . ,(command-stdout command)))
          '())))

(define (js-expression->cwl expression port)
  "Render @var{expression}, a @code{<js-expression>} object, to
@var{port} as a CWL YAML specification."
  (scm->yaml (js-expression->cwl-scm expression)
             port))

(define (js-expression->cwl-scm expression)
  "Render @var{expression}, a @code{<js-expression>} object, into
a CWL tree."
  `((cwlVersion . ,%cwl-version)
    (class . ExpressionTool)
    (requirements
     (InlineJavascriptRequirement)
     ,@(staging-requirements (js-expression-inputs expression))
     ,@(js-expression-requirements expression))
    ,@(js-expression-other expression)
    (inputs . ,(map input->cwl-scm
                    (js-expression-inputs expression)))
    (outputs . ,(map output->cwl-scm
                     (js-expression-outputs expression)))
    (expression . ,(js-expression-expression expression))))
