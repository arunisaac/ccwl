;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2022, 2023, 2025 Arun Isaac <arunisaac@systemreboot.net>
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

(use-modules (rnrs conditions)
             (rnrs exceptions)
             (srfi srfi-1)
             (srfi srfi-64)
             (srfi srfi-71)
             (ccwl conditions)
             (ccwl utils))

(define plist-ref
  (@@ (ccwl utils) plist-ref))

(test-begin "utils")

(test-equal "pairify"
  '((1 . 2) (3 . 4) (5 . 6))
  (pairify (list 1 2 3 4 5 6)))

(test-equal "plist-ref"
  2
  (plist-ref (list #:spam 1 #:ham 2 #:eggs 3)
             #:ham))

(test-equal "plist-ref with absent key"
  #f
  (plist-ref (list #:spam 1 #:ham 2 #:eggs 3)
             #:foo))

(test-equal "group-keyword-arguments"
  '(#:spam 1 #:ham (1 2 3) #:eggs (0))
  ((@@ (ccwl utils) group-keyword-arguments)
   (list #:spam 1 #:ham 1 2 3 #:eggs 0)
   (list #:spam)))

;; We cannot use test-equal to compare syntax objects, since
;; test-equal does not preserve the lexical contexts of the test
;; expressions.
(test-assert "unsyntax-keywords"
  (equal? (list #:ham #'1 #:eggs #'2)
          ((@@ (ccwl utils) unsyntax-keywords)
           (list #'#:ham #'1 #'#:eggs #'2))))

(test-equal "lambda**"
  '(1 2 123 (1 2 3))
  ((lambda** (a b #:key foo #:key* bar)
     (list a b foo bar))
   1 2 #:foo 123 #:bar 1 2 3))

(test-equal "lambda** with default values"
  '(1 2 123 9 (321 456) (7) (3 2 1))
  ((lambda** (foo aal #:key vale (pal 9) #:key* naal (irandu 7) (sol 3 2 1))
     (list foo aal vale pal naal irandu sol))
   1 2 #:vale 123 #:naal 321 456))

(test-equal "default default value of lambda** unary argument should be #f"
  #f
  ((lambda** (#:key foo)
     foo)))

(test-equal "default default value of lambda** n-ary argument should be the empty list"
  '()
  ((lambda** (#:key* foo)
     foo)))

(test-assert "lambda** should raise an &unrecognized-keyword-assertion on unrecognized keywords in arguments with syntax objects as irritants"
  (guard (exception
          (else (and (unrecognized-keyword-assertion? exception)
                     ;; We check with NOT keyword? because we have no
                     ;; way of directly checking for syntax?.
                     (not (any keyword? (condition-irritants exception))))))
    (macroexpand
     '(lambda** (#:key foo #:foo bar)
        foo))))

(test-equal "Allow other keys in lambda**"
  1
  ((lambda** (#:key foo #:allow-other-keys)
     foo)
   #:foo 1 #:bar 2))

(test-assert "Unrecognized keyword argument passed to lambda** should raise an &unrecognized-keyword-assertion condition"
  (guard (exception
          (else (unrecognized-keyword-assertion? exception)))
    ((lambda** (spam ham #:key eggs)
       spam)
     1 2 #:foo 123)))

(test-assert "Unary lambda** keyword argument passed multiple arguments should raise an &invalid-keyword-arity-assertion condition"
  (guard (exception
          (else (invalid-keyword-arity-assertion? exception)))
    ((lambda** (spam ham #:key eggs)
       (list spam ham eggs))
     1 2 #:eggs 123 345)))

(test-assert "Wrong number of positional arguments to lambda** should raise an &invalid-positional-arguments-arity-assertion condition"
  (guard (exception
          (else (invalid-positional-arguments-arity-assertion? exception)))
    ((lambda** (spam ham #:key eggs)
       spam)
     1 #:eggs 123)))

(test-assert "syntax-lambda**"
  (equal? (list #'1 #'2 #'123 (list #'1 #'2 #'3))
          ((syntax-lambda** (a b #:key foo #:key* bar)
             (list a b foo bar))
           #'1 #'2 #'#:foo #'123 #'#:bar #'1 #'2 #'3)))

(test-assert "syntax-lambda** with default values"
  (equal? (list #'1 #'2 #'123 9 #'(321 456) '(7) '(3 2 1))
          ((syntax-lambda** (foo aal #:key vale (pal 9) #:key* naal (irandu 7) (sol 3 2 1))
             (list foo aal vale pal naal irandu sol))
           #'1 #'2 #'#:vale #'123 #'#:naal #'321 #'456)))

(test-equal "default default value of syntax-lambda** unary argument should be #f"
  #f
  ((syntax-lambda** (#:key foo)
     foo)))

(test-equal "default default value of syntax-lambda** n-ary argument should be the empty list"
  '()
  ((syntax-lambda** (#:key* foo)
     foo)))

;; We cannot use test-equal to compare syntax objects, since
;; test-equal does not preserve the lexical contexts of the test
;; expressions.
(test-assert "Allow other keys in syntax-lambda**"
  (equal? #'1
          ((syntax-lambda** (#:key foo #:allow-other-keys)
             foo)
           #'#:foo #'1 #'#:bar #'2)))

(test-assert "syntax-lambda** should raise an &unrecognized-keyword-assertion on unrecognized keywords in arguments"
  (guard (exception
          (else (unrecognized-keyword-assertion? exception)))
    (macroexpand
     '(syntax-lambda** (#:key foo #:foo bar)
        foo))))

(test-assert "Unrecognized keyword argument passed to syntax-lambda** should raise an &unrecognized-keyword-assertion condition with syntax objects as irritants"
  (guard (exception
          (else (and (unrecognized-keyword-assertion? exception)
                     ;; We check with NOT keyword? because we have no
                     ;; way of directly checking for syntax?.
                     (not (any keyword? (condition-irritants exception))))))
    ((syntax-lambda** (spam ham #:key eggs)
       spam)
     #'1 #'2 #'#:foo #'123)))

(test-assert "Unary syntax-lambda** keyword argument passed multiple arguments should raise an &invalid-keyword-arity-assertion condition"
  (guard (exception
          (else (and (invalid-keyword-arity-assertion? exception)
                     ;; We check with NOT keyword? because we have no
                     ;; way of directly checking for syntax?.
                     (not (any keyword? (condition-irritants exception))))))
    ((syntax-lambda** (spam ham #:key eggs)
       (list spam ham eggs))
     #'1 #'2 #'#:eggs #'123 #'345)))

(test-assert "Wrong number of positional arguments to syntax-lambda** should raise an &invalid-positional-arguments-arity-assertion condition"
  (guard (exception
          (else (invalid-positional-arguments-arity-assertion? exception)))
    ((syntax-lambda** (spam ham #:key eggs)
       spam)
     #'1 #'#:eggs #'123)))

(test-equal "filter-mapi"
  '(1 3 5 7 9)
  (filter-mapi (lambda (item index)
                 (and (even? index)
                      (1+ item)))
               (iota 10)))

(test-equal "mapn"
  '((0 1 4 9 16)
    (0 1 8 27 64))
  (let ((squares cubes (mapn (lambda (n)
                               (values (expt n 2)
                                       (expt n 3)))
                             (iota 5)
                             2)))
    (list squares cubes)))

(test-equal "mapn on an empty list"
  '(() ())
  (let ((squares cubes (mapn (lambda (n)
                               (values (expt n 2)
                                       (expt n 3)))
                             '()
                             2)))
    (list squares cubes)))

(test-equal "foldn"
  '(45 285)
  (let ((sum sum-of-squares
             (foldn (lambda (n sum sum-of-squares)
                      (values (+ sum n)
                              (+ sum-of-squares (expt n 2))))
                    (iota 10)
                    0 0)))
    (list sum sum-of-squares)))

(test-equal "pairify must ignore extra elements when list has an odd number of elements"
  '((1 . 2) (3 . 4) (5 . 6))
  (pairify (list 1 2 3 4 5 6 7)))

(test-end "utils")
