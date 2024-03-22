;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2023–2024 Arun Isaac <arunisaac@systemreboot.net>
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
  #:export (workflow->dot
            command->dot
            function->dot))

(define function->dot
  (match-lambda*
    (((? workflow? workflow) port)
     (workflow->dot workflow port))
    (((? command? command) port)
     (command->dot command port))
    (((? js-expression? expression) port)
     (js-expression->dot expression port))))

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

(define-immutable-record-type <graph-edge>
  (make-graph-edge from to properties)
  graph-edge?
  (from graph-edge-from)
  (to graph-edge-to)
  (properties graph-edge-properties))

(define* (graph-edge from to #:optional (properties '()))
  "Construct <graph-edge> object."
  (make-graph-edge from to properties))

(define-immutable-record-type <graph-port>
  (graph-port node port)
  graph-port?
  (node graph-port-node)
  (port graph-port-name))

(define-immutable-record-type <html-string>
  (html-string str)
  html-string?
  (str html-string-underlying))

(define (workflow->graph workflow)
  "Convert WORKFLOW, a <workflow> object, to a <graph> object."
  (graph 'workflow
         #:properties '((bgcolor . "#eeeeee"))
         #:nodes (map (compose step-node step-id)
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
         #:subgraphs (list (inputs-cluster (workflow-inputs workflow))
                           (outputs-cluster (workflow-outputs workflow)))))

(define (single-node-workflow->graph node-name inputs outputs)
  "Convert a single node workflow (usually a @code{<command>} or
@code{<js-expression>}) with @var{node-name}, @var{inputs} and
@var{outputs}, to a @code{<graph>} object."
  (graph 'workflow
         #:properties '((bgcolor . "#eeeeee"))
         #:nodes (list (graph-node node-name
                                   '((fillcolor . "lightgoldenrodyellow")
                                     (shape . "record")
                                     (style . "filled"))))
         #:edges (append
                  ;; Connect inputs to command.
                  (map (lambda (input)
                         (cons (input-id input)
                               node-name))
                       inputs)
                  ;; Connect command to outputs.
                  (map (lambda (output)
                         (cons node-name
                               (output-id output)))
                       outputs))
         #:subgraphs (list (inputs-cluster inputs)
                           (outputs-cluster outputs))))

(define (command->dot command port)
  "Render @var{command}, a @code{<command>} object, to @var{port} in the
graphviz dot language."
  (graph->dot (command->graph command)
              port))

(define (command->graph command)
  "Convert @var{command}, a @code{<command>} object, to a @code{<graph>}
object."
  (single-node-workflow->graph 'command
                               (command-inputs command)
                               (command-outputs command)))

(define (js-expression->dot expression port)
  "Render @var{expression}, a @code{<js-expression>} object, to
@var{port} in the graphviz dot language."
  (graph->dot (js-expression->graph expression)
              port))

(define (js-expression->graph expression)
  "Convert @var{expression}, a @code{<js-expression>} object, to a
@code{<graph>} object."
  (single-node-workflow->graph 'js-expression
                               (js-expression-inputs expression)
                               (js-expression-outputs expression)))

(define (step-node id)
  "Return graph node describing step with @var{id}."
  (graph-node id
              '((fillcolor . "lightgoldenrodyellow")
                (shape . "record")
                (style . "filled"))))

(define (inputs-cluster inputs)
  "Return the subgraph clustering @var{inputs}."
  (graph 'cluster_inputs
         #:properties '((label . "Workflow Inputs")
                        (rank . "same")
                        (style . "dashed"))
         #:nodes (map (lambda (input)
                        (graph-node (input-id input)
                                    '((fillcolor . "#94ddf4")
                                      (shape . "record")
                                      (style . "filled"))))
                      inputs)))

(define (outputs-cluster outputs)
  "Return the subgraph clustering @var{outputs}."
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
                      outputs)))

(define (serialize object)
  "Serialize OBJECT according to graphviz dot syntax. OBJECT may a
symbol, a string, a <graph-port> object, or a <html-string> object."
  (cond
   ((symbol? object)
    (serialize (symbol->string object)))
   ((graph-port? object)
    (string-append (serialize (graph-port-node object))
                   ":"
                   (serialize (graph-port-name object))))
   ;; Surround HTML strings in <>, and don't escape.
   ((html-string? object)
    (format "<~a>" (html-string-underlying object)))
   ;; Don't quote safe strings.
   ((and (string? object)
         (string-every (char-set-union (char-set-intersection char-set:letter+digit
                                                              char-set:ascii)
                                       (char-set #\_))
                       object))
    object)
   ;; Quote strings with unsafe characters.
   ((string? object)
    (format "\"~a\"" (string-replace-substring object "\"" "\\\"")))
   (else (error "Unknown object type to serialize to graphviz dot:" object))))

(define (serialize-properties properties)
  (string-append
   "["
   (string-join (map (match-lambda
                       ((key . value)
                        (format "~a=~a" key (serialize value))))
                     properties)
                ", ")
   "]"))

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
                (display " " port)
                (display (serialize-properties (graph-node-properties node))
                         port))
              (display (format ";~%") port))
            (graph-nodes graph))
  (for-each (lambda (edge)
              (indent-level port (1+ level))
              (match edge
                ((? graph-edge? edge)
                 (display (format "~a -> ~a"
                                  (serialize (graph-edge-from edge))
                                  (serialize (graph-edge-to edge)))
                          port)
                 (unless (null? (graph-edge-properties edge))
                   (display " " port)
                   (display (serialize-properties (graph-edge-properties edge))
                            port))
                 (display (format ";~%") port))
                ((from . to)
                 (display (format "~a -> ~a;~%"
                                  (serialize from)
                                  (serialize to))
                          port))))
            (graph-edges graph))
  (for-each (lambda (subgraph)
              (graph->dot subgraph port (1+ level)))
            (graph-subgraphs graph))
  (indent-level port level)
  (display (format "}~%") port))
