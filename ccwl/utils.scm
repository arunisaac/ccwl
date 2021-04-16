(define-module (ccwl utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export (group-arguments))

(define (group-keyword-arguments args unary-keywords)
  "Group ARGS, a list of keyword arguments of arbitrary arity. Return
a list of unary keyword arguments. n-ary arguments are grouped
together into lists. Keywords that are to be treated as having unit
arity are listed in UNARY-KEYWORDS."
  (match args
    (((? (lambda (keyword)
           (and (keyword? keyword)
                (not (member keyword unary-keywords))))
         this-keyword)
      tail ...)
     (let ((this-keyword-args tail (break keyword? tail)))
       (cons* this-keyword
              (apply list this-keyword-args)
              (group-keyword-arguments tail unary-keywords))))
    (((? (lambda (keyword)
           (and (keyword? keyword)
                (member keyword unary-keywords)))
         this-keyword)
      this-keyword-arg tail ...)
     (cons* this-keyword this-keyword-arg
            (group-keyword-arguments tail unary-keywords)))
    ((non-keyword _ ...)
     (error "Invalid sequence of keywords arguments" args))
    (() '())))

(define* (group-arguments args #:optional (unary-keywords '()))
  "Group ARGS, a list of positional arguments followed by keyword
arguments of arbitrary arity. Return a list of positional arguments
followed by unary keyword arguments. n-ary arguments are grouped
together into lists. Keywords that are to be treated as having unit
arity are listed in UNARY-KEYWORDS."
  (let ((positional-arguments keyword-arguments (break keyword? args)))
    (append positional-arguments
            (group-keyword-arguments keyword-arguments unary-keywords))))
(define (plist-ref plist key)
  "Return the value from the first entry in PLIST with the given KEY,
or #f if there is no such entry."
  (match (find-tail (cut eq? key <>) plist)
    ((_ value . _) value)
    (#f #f)))

