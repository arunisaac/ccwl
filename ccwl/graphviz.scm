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
;; <command>, <input>, <output>, <step>) to the graphviz dot language.

;;; Code:

(define-module (ccwl graphviz)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-28)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (ccwl ccwl)
  #:use-module (ccwl utils)
  #:export (workflow->dot))

(define (workflow->dot workflow port)
  "Render WORKFLOW, a <workflow> object, to PORT in the graphviz dot
language."
  (graph->dot (workflow->graph workflow)
              port))

(define-immutable-record-type <graph>
  (make-graph name properties nodes edges subgraphs)
  graph?
  (name graph-name)
  (properties graph-properties)
  (nodes graph-nodes)
  (edges graph-edges)
  (subgraphs graph-subgraphs))

(define* (graph name #:key (properties '()) (nodes '()) (edges '()) (subgraphs '()))
  "Construct <graph> object."
  (make-graph name properties nodes edges subgraphs))

(define-immutable-record-type <graph-node>
  (make-graph-node name properties)
  graph-node?
  (name graph-node-name)
  (properties graph-node-properties))

(define* (graph-node name #:optional (properties '()))
  "Construct <graph-node> object."
  (make-graph-node name properties))

(define-immutable-record-type <html-string>
  (html-string str)
  html-string?
  (str html-string-underlying))

(define (workflow->graph workflow)
  "Convert WORKFLOW, a <workflow> object, to a <graph> object."
  (graph 'workflow
         #:properties '((bgcolor . "#eeeeee"))
         #:nodes (map (lambda (step)
                        (graph-node (step-id step)
                                    '((fillcolor . "lightgoldenrodyellow")
                                      (shape . "record")
                                      (style . "filled"))))
                      (workflow-steps workflow))
         #:edges (append
                  ;; Connect steps and inputs to steps.
                  (append-map (lambda (step)
                                (map (match-lambda
                                       ((_ . address)
                                        (cons (string->symbol
                                               (match (string-split address #\/)
                                                 ((step _) step)
                                                 ((workflow-input) workflow-input)))
                                              (step-id step))))
                                     (step-in step)))
                              (workflow-steps workflow))
                  ;; Connect output sources to outputs.
                  (map (lambda (output)
                         (cons (match (string-split (output-source output) #\/)
                                 ((source _) source))
                               (output-id output)))
                       (workflow-outputs workflow)))
         #:subgraphs (list (graph 'cluster_inputs
                                  #:properties '((label . "Workflow Inputs")
                                                 (rank . "same")
                                                 (style . "dashed"))
                                  #:nodes (map (lambda (input)
                                                 (graph-node (input-id input)
                                                             '((fillcolor . "#94ddf4")
                                                               (shape . "record")
                                                               (style . "filled"))))
                                               (workflow-inputs workflow)))
                           (graph 'cluster_outputs
                                  #:properties '((label . "Workflow Outputs")
                                                 (labelloc . "b")
                                                 (rank . "same")
                                                 (style . "dashed"))
                                  #:nodes (map (lambda (output)
                                                 (graph-node (output-id output)
                                                             '((fillcolor . "#94ddf4")
                                                               (shape . "record")
                                                               (style . "filled"))))
                                               (workflow-outputs workflow))))))

(define (serialize object)
  "Serialize OBJECT according to graphviz dot syntax. OBJECT may a
symbol, a string, or a <html-string> object."
  (let ((str (cond
              ((symbol? object) (symbol->string object))
              ((string? object) object)
              ((html-string? object) (html-string-underlying object))
              (else (error "Unknown object type to serialize to graphviz dot:" object)))))
    (cond
     ;; Surround HTML strings in <>, and don't escape.
     ((html-string? object)
      (format "<~a>" str))
     ;; Don't quote safe strings.
     ((string-every (char-set-union (char-set-intersection char-set:letter+digit
                                                           char-set:ascii)
                                    (char-set #\_))
                    str)
      str)
     ;; Quote strings with unsafe characters.
     (else
      (format "\"~a\"" (string-replace-substring str "\"" "\\\""))))))

(define* (graph->dot graph #:optional (port (current-output-port)) (level 0))
  "Render GRAPH, a <graph> object, in the graphviz dot syntax to
PORT."
  (indent-level port level)
  (display (format "~a ~a {~%"
                   (if (zero? level) "digraph" "subgraph")
                   (graph-name graph))
           port)
  (for-each (match-lambda
              ((key . value)
               (indent-level port (1+ level))
               (display (format "~a=~a;~%" key (serialize value)) port)))
            (graph-properties graph))
  (for-each (lambda (node)
              (indent-level port (1+ level))
              (display (serialize (graph-node-name node)) port)
              (unless (null? (graph-node-properties node))
                (display (format " [~a]"
                                 (string-join (map (match-lambda
                                                     ((key . value)
                                                      (format "~a=~a" key (serialize value))))
                                                   (graph-node-properties node))
                                              ", "))
                         port))
              (display (format ";~%") port))
            (graph-nodes graph))
  (for-each (match-lambda
              ((from . to)
               (indent-level port (1+ level))
               (display (format "~a -> ~a;~%"
                                (serialize from)
                                (serialize to))
                        port)))
            (graph-edges graph))
  (for-each (lambda (subgraph)
              (graph->dot subgraph port (1+ level)))
            (graph-subgraphs graph))
  (indent-level port level)
  (display (format "}~%") port))
