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

(use-modules (srfi srfi-64))

(define graph->dot
  (@@ (ccwl graphviz) graph->dot))

(define graph
  (@@ (ccwl graphviz) graph))

(define graph-node
  (@@ (ccwl graphviz) graph-node))

(define graph-port
  (@@ (ccwl graphviz) graph-port))

(define html-string
  (@@ (ccwl graphviz) html-string))

(test-begin "graphviz")

(test-equal "serialize HTML strings correctly"
  "digraph foo {
  bar [label=<<table><tr><td>bar</td></tr></table>>];
}
"
  (call-with-output-string
    (lambda (port)
      (graph->dot
       (graph 'foo
              #:nodes (list (graph-node 'bar
                                        `((label . ,(html-string "<table><tr><td>bar</td></tr></table>"))))))
       port))))

(test-equal "do not escape backslashes"
  "digraph foo {
  bar [label=\"foo\\lbar\"];
}
"
  (call-with-output-string
    (lambda (port)
      (graph->dot
       (graph 'foo
              #:nodes (list (graph-node 'bar
                                        `((label . "foo\\lbar")))))
       port))))

(test-equal "serialize ports correctly"
  "digraph foo {
  foo:p1 -> bar:p2;
}
"
  (call-with-output-string
    (lambda (port)
      (graph->dot
       (graph 'foo
              #:edges `((,(graph-port "foo" "p1") . ,(graph-port "bar" "p2"))))
       port))))

(test-end "graphviz")
