;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2022 Arun Isaac <arunisaac@systemreboot.net>
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

(use-modules (srfi srfi-64)
             (srfi srfi-71)
             (ccwl utils))

(test-begin "utils")

(test-equal "pairify"
  '((1 . 2) (3 . 4) (5 . 6))
  (pairify (list 1 2 3 4 5 6)))

(test-equal "plist->alist"
  '((spam . 1) (ham . 2) (eggs . 3))
  (plist->alist (list #:spam 1 #:ham 2 #:eggs 3)))

(define plist-ref
  (@@ (ccwl utils) plist-ref))

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

(test-equal "default value of lambda** unary argument should be #f"
  #f
  ((lambda** (#:key foo)
     foo)))

(test-equal "default value of lambda** n-ary argument should be the empty list"
  '()
  ((lambda** (#:key* foo)
     foo)))

(test-error "lambda** should error out on unrecognized keywords in arguments" #t
  (macroexpand
   '(lambda** (#:key foo #:foo bar)
      foo)))

(test-equal "Allow other keys in lambda**"
  1
  ((lambda** (#:key foo #:allow-other-keys)
     foo)
   #:foo 1 #:bar 2))

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

;; We cannot use test-equal to compare syntax objects, since
;; test-equal does not preserve the lexical contexts of the test
;; expressions.
(test-assert "Allow other keys in syntax-lambda**"
  (equal? #'1
          ((syntax-lambda** (#:key foo #:allow-other-keys)
             foo)
           #'#:foo #'1 #'#:bar #'2)))

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
                             (iota 5))))
    (list squares cubes)))

(test-equal "append-mapn"
  '((0 0 1 1 2 4 3 9 4 16)
    (0 0 1 1 2 8 3 27 4 64))
  (let ((squares cubes (append-mapn (lambda (n)
                                      (values (list n (expt n 2))
                                              (list n (expt n 3))))
                                    (iota 5))))
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

(test-end "utils")
