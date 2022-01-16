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

;;; Commentary:

;; A few useful utilities

;;; Code:

(define-module (ccwl utils)
  #:use-module ((rnrs conditions) #:select (condition
                                            condition-irritants
                                            (make-irritants-condition . irritants-condition)))
  #:use-module ((rnrs exceptions) #:select (guard
                                            (raise . raise-exception)))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ccwl conditions)
  #:export (indent-level
            pairify
            plist->alist
            lambda**
            syntax-lambda**
            mapn
            append-mapn
            foldn
            filter-mapi))

(define (indent-level port level)
  "Emit whitespaces to PORT corresponding to nesting LEVEL."
  (display (make-string (* 2 level) #\space) port))

(define (pairify lst)
  "Return a list of pairs of successive elements of LST. For example,

(pairify (list 1 2 3 4 5 6))
=> ((1 . 2) (3 . 4) (5 . 6))"
  (match lst
    (() '())
    ((first second tail ...)
     (cons (cons first second)
           (pairify tail)))))

(define* (group-keyword-arguments args #:optional (unary-keywords (list)))
  "Group ARGS, a list of keyword arguments of arbitrary arity. Return
a list of unary keyword arguments. n-ary arguments are grouped
together into lists. Keywords that are to be treated as having unit
arity are listed in UNARY-KEYWORDS. For example,

(group-keyword-arguments (list #:spam 1 #:ham 1 2 3 #:eggs 0)
                         (list #:spam))
=> (#:spam 1 #:ham (1 2 3) #:eggs (0))

If a unary keyword is passed multiple arguments, a
&invalid-keyword-arity-assertion is raised."
  (match args
    (((? keyword? this-keyword) tail ...)
     (let ((this-keyword-args tail (break keyword? tail)))
       (cons* this-keyword
              (if (member this-keyword unary-keywords)
                  ;; unary keyword argument
                  (match this-keyword-args
                    ((this-keyword-arg) this-keyword-arg)
                    (_ (raise-exception
                        (condition (invalid-keyword-arity-assertion)
                                   (irritants-condition
                                    (cons this-keyword this-keyword-args))))))
                  ;; n-ary keyword argument
                  (apply list this-keyword-args))
              (group-keyword-arguments tail unary-keywords))))
    ((non-keyword _ ...)
     (error "Invalid sequence of keywords arguments" args))
    (() '())))

(define (plist-ref plist key)
  "Return the value from the first entry in PLIST with the given KEY,
or #f if there is no such entry. For example,

(plist-ref (list #:spam 1 #:ham 2 #:eggs 3)
           #:ham)
=> 2

(plist-ref (list #:spam 1 #:ham 2 #:eggs 3)
           #:foo)
=> #f"
  (match (find-tail (cut eq? key <>) plist)
    ((_ value . _) value)
    (#f #f)))

(define (unsyntax-keywords lst)
  "Unsyntax keywords in LST, a list of syntax objects. For example:

(unsyntax-keywords (list #'#:ham #'1 #'#:eggs #'2))
=> (#:ham #'1 #:eggs #'2)"
  (map (lambda (element)
         (if (keyword? (syntax->datum element))
             (syntax->datum element)
             element))
       lst))

;; TODO: Implement a define** for lambda** in the spirit of define*
;; for lambda*.
(define-syntax lambda**
  (lambda (x)
    "Define a lambda function that can have positional arguments
followed by unary and n-ary keyword arguments. Unary keyword arguments
are prefixed by #:key. n-ary keyword arguments are prefixed by
#:key*. For example:

(lambda** (a b #:key foo #:key* bar)
  (list a b foo bar))

Here, a and b are positional arguments. foo is a unary keyword
argument. bar is an n-ary keyword argument. The above function could,
for example, be invoked as:

((lambda** (a b #:key foo #:key* bar)
   (list a b foo bar))
 1 2 #:foo 123 #:bar 1 2 3)

=> (1 2 123 (1 2 3))

lambda** also supports default values for both unary and n-ary keyword
arguments. Note that the default default value for unary arguments is
#f, while that for n-ary arguments is the empty list. For example,

((lambda** (foo bar #:key aal vale (pal 9) #:key* naal (irandu 7) (sol 3 2 1) uruthi)
   (list foo bar aal vale pal naal irandu sol uruthi))
 1 2 #:vale 123 #:naal 321 456)
=> (1 2 #f 123 9 (321 456) (7) (3 2 1) ())

Like lambda*, lambda** supports #:allow-other-keys. For example,

((lambda** (#:key foo #:allow-other-keys)
   foo)
 #:foo 1 #:bar 2)
=> 1

However, #:optional and #:rest are not supported.

If an unrecognized keyword is passed to the lambda function, a
&unrecognized-keyword-assertion condition is raised. If a unary
keyword argument is passed more than one argument, a
&invalid-keyword-arity-assertion condition is raised. If a wrong
number of positional arguments is passed, a
&invalid-positional-arguments-arity-assertion condition is raised."
    (syntax-case x ()
      ((_ (args-spec ...) body ...)
       #`(lambda args
           #,(let* ((positionals rest (break keyword?
                                             (unsyntax-keywords #'(args-spec ...))))
                    (grouped-rest (group-keyword-arguments rest))
                    (unary-arguments (or (plist-ref grouped-rest #:key)
                                         (list)))
                    (nary-arguments (or (plist-ref grouped-rest #:key*)
                                        (list)))
                    (allow-other-keys? (if (plist-ref grouped-rest #:allow-other-keys)
                                           #t #f)))
               (let ((unrecognized-keywords
                      (lset-difference (lambda (x y)
                                         (let ((x (if (keyword? x) x (syntax->datum x)))
                                               (y (if (keyword? y) y (syntax->datum x))))
                                           (eq? x y)))
                                       (filter (compose keyword? syntax->datum)
                                               #'(args-spec ...))
                                       (list #:key #:key* #:allow-other-keys))))
                 (unless (null? unrecognized-keywords)
                   (raise-exception
                    (condition (unrecognized-keyword-assertion)
                               (irritants-condition unrecognized-keywords)))))
               #`(apply (lambda* #,(append positionals
                                           (cons #:key unary-arguments)
                                           (map (lambda (x)
                                                  (syntax-case x ()
                                                    ((arg defaults ...)
                                                     #'(arg (list defaults ...)))
                                                    (arg #'(arg '()))))
                                                nary-arguments)
                                           (if allow-other-keys?
                                               (list #:allow-other-keys)
                                               (list)))
                          body ...)
                        (let ((positionals rest (break keyword? args)))
                          ;; Test for correct number of positional
                          ;; arguments.
                          (unless (= (length positionals)
                                     #,(length positionals))
                            (raise-exception
                             (condition (invalid-positional-arguments-arity-assertion)
                                        (irritants-condition positionals))))
                          ;; Test if all keywords are recognized.
                          (let ((unrecognized-keywords
                                 (lset-difference eq?
                                                  (filter keyword? rest)
                                                  '#,(map (lambda (x)
                                                            (symbol->keyword
                                                             (syntax->datum (syntax-case x ()
                                                                              ((arg _ ...) #'arg)
                                                                              (arg #'arg)))))
                                                          (append unary-arguments
                                                                  nary-arguments)))))
                            (unless (or #,allow-other-keys?
                                        (null? unrecognized-keywords))
                              (raise-exception
                               (condition (unrecognized-keyword-assertion)
                                          (irritants-condition unrecognized-keywords)))))
                          (append positionals
                                  (group-keyword-arguments
                                   rest (list #,@(map (lambda (x)
                                                        (symbol->keyword
                                                         (syntax->datum
                                                          (syntax-case x ()
                                                            ((arg default) #'arg)
                                                            (arg #'arg)))))
                                                      unary-arguments))))))))))))

(define-syntax-rule (syntax-lambda** formal-args body ...)
  "Like lambda**, but for syntax objects. For example,

((syntax-lambda** (a b #:key foo #:key* bar)
   (list a b foo bar))
 #'1 #'2 #'#:foo #'123 #'#:bar #'1 #'2 #'3)
=> (#'1 #'2 #'123 (#'1 #'2 #'3))

Like lambda**, syntax-lambda** supports default values for
arguments. The default default value for unary arguments is #f, while
that for n-ary arguments is the empty list. For example,

((syntax-lambda** (foo aal #:key vale (pal 9) #:key* naal (irandu 7) (sol 3 2 1))
   (list foo aal vale pal naal irandu sol))
 #'1 #'2 #'#:vale #'123 #'#:naal #'321 #'456)
=> (#'1 #'2 #'123 9 (#'321 #'456) (7) (3 2 1))

Like lambda**, syntax-lambda** supports #:allow-other-keys.

((syntax-lambda** (#:key foo #:allow-other-keys)
     foo)
   #'#:foo #'1 #'#:bar #'2)
=> #'1

#:optional and #:rest are not supported.

If an unrecognized keyword is passed to the lambda function, a
&unrecognized-keyword-assertion condition is raised. If a unary
keyword argument is passed more than one argument, a
&invalid-keyword-arity-assertion condition is raised. If a wrong
number of positional arguments is passed, a
&invalid-positional-arguments-arity-assertion condition is raised."
  (lambda args
    (guard (exception
            ((unrecognized-keyword-assertion? exception)
             (raise-exception
              (condition (unrecognized-keyword-assertion)
                         (irritants-condition
                          ;; Resyntax irritant keywords.
                          (map (lambda (irritant-keyword)
                                 (find (lambda (arg)
                                         (eq? (syntax->datum arg)
                                              irritant-keyword))
                                       args))
                               (condition-irritants exception))))))
            ((invalid-keyword-arity-assertion? exception)
             (raise-exception
              (condition (invalid-keyword-arity-assertion)
                         (irritants-condition
                          ;; Resyntax irritant keyword.
                          (match (condition-irritants exception)
                            ((irritant-keyword . irritant-args)
                             (cons (find (lambda (arg)
                                           (eq? (syntax->datum arg)
                                                irritant-keyword))
                                         args)
                                   irritant-args))))))))
      (apply
       (lambda** formal-args body ...)
       (unsyntax-keywords args)))))

(define (filter-mapi proc lst)
  "Indexed filter-map. Like filter-map, but PROC calls are (proc item
index) where ITEM is an element of list and INDEX is the index of that
element. For example,

(filter-mapi (lambda (item index)
               (and (even? index)
                    (1+ item)))
             (iota 10))
=> (1 3 5 7 9)"
  (filter-map (lambda (item index)
                (proc item index))
              lst
              (iota (length lst))))

(define (mapn proc lst)
  "Map the procedure PROC over list LST and return a list containing
the results. PROC can return multiple values, in which case, an equal
number of lists are returned. For example,

(mapn (lambda (n)
        (values (expt n 2)
                (expt n 3)))
      (iota 5))
=> (0 1 4 9 16)
=> (0 1 8 27 64)"
  (apply values
         (apply zip
                (map (lambda (x)
                       (call-with-values (cut proc x) list))
                     lst))))

(define (append-mapn proc lst)
  "Map PROC over LST just as in mapn, but append the results
together. PROC can return multiple values, in which case, an equal
number of lists are returned.

(append-mapn (lambda (n)
               (values (list n (expt n 2))
                       (list n (expt n 3))))
      (iota 5))
=> (0 0 1 1 2 4 3 9 4 16)
=> (0 0 1 1 2 8 3 27 4 64)"
  (call-with-values (cut mapn proc lst)
    (lambda lists
      (apply values
             (map (lambda (lst)
                    (apply append lst))
                  lists)))))

(define (foldn proc lst . inits)
  "Apply PROC to the elements of LST to build a result, and return
that result. PROC can return multiple values, in which case, an equal
number of values are returned. Each PROC call is (PROC ELEMENT
PREVIOUS ...) where ELEMENT is an element of LST, and (PREVIOUS ...)
is the return from the previous call to PROC or the given INITS for
the first call. For example,

(foldn (lambda (n sum sum-of-squares)
         (values (+ sum n)
                 (+ sum-of-squares (expt n 2))))
       (iota 10)
       0 0)
=> 45
=> 285"
  (apply values
         (fold (lambda (element results)
                 (call-with-values (cut apply proc element results) list))
               inits
               lst)))
