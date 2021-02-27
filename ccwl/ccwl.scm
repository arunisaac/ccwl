;;
;; CWL generator
;;
;; This file implements a generator to generate CWL files.

(define-module (ccwl ccwl)
  #:use-module (rnrs records syntactic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export (clitool
            workflow
            input
            output
            step
            workflow-output
            intermediate
            clitool-step))

(define-record-type (<input> make-input input?)
  (fields (immutable id input-id)
          (immutable type input-type)
          (immutable default input-default)
          (immutable label input-label)
          (immutable other input-other)))

(define-record-type unspecified-default)

(define* (input id #:key type label (default (make-unspecified-default)) (other '()))
  "Build and return an <input> object."
  (make-input id type default label other))

(define-record-type (<output> make-output output?)
  (fields (immutable id output-id)
          (immutable type output-type)
          (immutable binding output-binding)
          (immutable other output-other)))

(define* (output id #:key type binding (other '()))
  "Build and return an <output> object."
  (make-output id type binding other))

(define-record-type (<intermediate> intermediate intermediate?)
  (fields (immutable input intermediate-input)
          (immutable output-source intermediate-output-source)))

(define* (clitool-step id args #:key (additional-inputs '()) (outputs '()) stdout stderr (other '()))
  (step id
        (clitool (map (lambda (arg)
                        (if (intermediate? arg)
                            (intermediate-input arg)
                            arg))
                      args)
                 #:additional-inputs additional-inputs
                 #:outputs outputs
                 #:stdout stdout
                 #:stderr stderr
                 #:other other)
        (append (filter (lambda (arg)
                          (or (input? arg)
                              (intermediate? arg)))
                        args)
                additional-inputs)
        (map output-id outputs)))

(define* (parse-arguments args #:optional (position 1))
  "Parse ARGS, a list of command line arguments and return a parse
tree of labelled arguments. POSITION is an internal recursion
variable."
  (match args
    (((? string? head) tail ...)
     (if (string-prefix? "-" head)
         (match tail
           ((tail-head tail ...)
            (cons (list 'keyword head tail-head)
                  (parse-arguments tail position))))
         (error "Unrecognized argument" head)))
    ((head tail ...)
     (cons (list 'positional position head)
           (parse-arguments tail (1+ position))))
    (() '())))

(define (break-pair pred lst)
  "Return the longest initial prefix of LST that does not satisfy PRED,
and the remaining tail. PRED is a 2-arity predicate. For each element
under consideration, PRED is passed that element and the next. For the
last element of LST, PRED is passed that element alone."
  (match lst
    ((head next tail ...)
     (if (not (pred head next))
         (let ((prefix tail (break-pair pred (cons next tail))))
           (values (cons head prefix) tail))
         (values (list) lst)))
    ((last)
     (if (not (pred last))
         (values lst (list))
         (values (list) lst)))))

(define (parse-command args)
  "Parse ARGS, a list of command line arguments and return two
lists---the base command and the actual arguments."
  (let ((base-command arguments
                      (break-pair (case-lambda
                                    ((arg next)
                                     (and (string? arg)
                                          (string-prefix? "-" arg)
                                          (input? next)))
                                    ((last-arg)
                                     (input? last-arg)))
                                  args)))
    (values base-command
            (parse-arguments arguments))))

(define (input->tree input)
  "Convert INPUT, an <input> object, to a tree."
  `(,(input-id input)
    ,@(filter identity
              (list (and (input-type input)
                         (cons 'type (input-type input)))
                    (and (input-label input)
                         (cons 'label (input-label input)))
                    (and (not (unspecified-default? (input-default input)))
                         (cons 'default (input-default input)))))
    ,@(input-other input)))

(define* (clitool args #:key (additional-inputs '()) (outputs '()) stdout stderr (other '()))
  "Build a CommandLineTool class CWL workflow."
  (let ((base-command arguments (parse-command args)))
    `((cwl-version . "v1.1")
      (class . Command-line-tool)
      ,@other
      (base-command . ,(list->vector base-command))
      ,@(let ((inputs (append arguments additional-inputs)))
          (if (not (null? inputs))
              `((inputs . ,(map (match-lambda
                                  (('keyword prefix input)
                                   (append (input->tree input)
                                           `((input-binding (prefix . ,prefix)))))
                                  (('positional position input)
                                   (append (input->tree input)
                                           `((input-binding (position . ,position)))))
                                  (input
                                   (input->tree input)))
                                inputs)))
              (list)))
      ,@(if (or (not (null? outputs)) stdout stderr)
            `((outputs . ,(map (lambda (output)
                                 `(,(output-id output)
                                   ,@(filter identity
                                             (list (and (output-type output)
                                                        (cons 'type (output-type output)))
                                                   (and (output-binding output)
                                                        (cons 'output-binding (output-binding output)))))
                                   ,@(output-other output)))
                               outputs)))
            (list))
      ,@(if stdout
            `((stdout . ,stdout))
            '())
      ,@(if stderr
            `((stderr . ,stderr))
            '()))))

(define-record-type (<workflow-output> make-workflow-output workflow-output?)
  (fields (immutable id workflow-output-id)
          (immutable type workflow-output-type)
          (immutable source workflow-output-source)
          (immutable other workflow-output-other)))

(define* (workflow-output id #:key type source (other '()))
  "Build and return a <workflow-output> object."
  (make-workflow-output id type source other))

(define-record-type (<step> step step?)
  (fields (immutable id step-id)
          (immutable run step-run)
          (immutable in step-in)
          (immutable out step-out)))

(define* (workflow steps outputs #:key (other '()))
  "Build a Workflow class CWL workflow."
  `((cwlVersion . "v1.1")
    (class . Workflow)
    ,@other
    (inputs . ,(delete-duplicates
                (map input->tree
                     (append
                      (append-map (lambda (step)
                                    (filter-map (match-lambda
                                                  ((id . (? input? input)) input)
                                                  ((? input? input) input)
                                                  (_ #f))
                                                (step-in step)))
                                  steps)
                      (filter-map (lambda (output)
                                    (and (input? (workflow-output-source output))
                                         (workflow-output-source output)))
                                  outputs)))))
    (outputs . ,(map (lambda (output)
                       `(,(workflow-output-id output)
                         (type . ,(workflow-output-type output))
                         (output-source . ,(match (workflow-output-source output)
                                             ((? string? source) source)
                                             ((? input? input) (input-id input))))))
                     outputs))
    (steps . ,(map (lambda (step)
                     `(,(step-id step)
                       (in . ,(map (match-lambda
                                     ((id . input)
                                      (cons id (if (input? input)
                                                   (input-id input)
                                                   input)))
                                     ((? input? input)
                                      (cons (input-id input) (input-id input)))
                                     ((? intermediate? intermediate)
                                      (cons (input-id (intermediate-input intermediate))
                                            (intermediate-output-source intermediate))))
                                   (step-in step)))
                       (out . ,(list->vector (step-out step)))
                       (run . ,(step-run step))))
                   steps))))
