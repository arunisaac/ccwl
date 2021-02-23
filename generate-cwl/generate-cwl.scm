;;
;; CWL generator
;;
;; This file implements a generator to generate CWL files.

(define-module (generate-cwl generate-cwl)
  #:use-module (rnrs records syntactic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export (clitool
            workflow
            input
            output
            step
            workflow-output))

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

(define (parse-command args)
  "Parse ARGS, a list of command line arguments and return two
lists---the base command and the actual arguments."
  (let ((base-command arguments
                      (break (match-lambda
                               ((arg next)
                                (and (string? arg)
                                     (string-prefix? "-" arg)
                                     (input? next))))
                             (map list args (drop args 1)))))
    (values (append (map (match-lambda
                           ((arg next) arg))
                         base-command)
                    (if (input? (last args))
                        (list)
                        (take-right args 1)))
            (parse-arguments
             (append (map (match-lambda
                            ((arg next) arg))
                          arguments)
                     (if (input? (last args))
                         (take-right args 1)
                         (list)))))))

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
                                                   input))))
                                   (step-in step)))
                       (out . ,(list->vector (step-out step)))
                       (run . ,(step-run step))))
                   steps))))
