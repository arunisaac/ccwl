;;
;; CWL generator
;;
;; This file implements a generator to generate CWL files.

(define-module (ccwl ccwl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (clitool
            workflow
            input
            output
            step
            clitool-step))


(define-immutable-record-type <input>
  (make-input id type label default binding source other)
  input?
  (id input-id)
  (type input-type)
  (label input-label)
  (default input-default)
  (other input-other))

(define-immutable-record-type <unspecified-default>
  (make-unspecified-default)
  unspecified-default?)

(define* (input id #:key type label (default (make-unspecified-default)) (other '()))
  "Build and return an <input> object."
  (make-input id type label default other))

(define-immutable-record-type <output>
  (make-output id type binding source other)
  output?
  (id output-id)
  (type output-type)
  (binding output-binding)
  (source output-source)
  (other output-other))

(define* (output id #:key type binding source (other '()))
  "Build and return an <output> object."

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

  (make-output id type binding source other))


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

(define* (workflow steps outputs #:key (other '()))
(define-immutable-record-type <step>
  (make-step id run in out)
  step?
  (id step-id)
  (run step-run set-step-run)
  (in step-in set-step-in)
  (out step-out set-step-out))

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
