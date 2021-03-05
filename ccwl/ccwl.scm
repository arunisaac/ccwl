;;
;; CWL generator
;;
;; This file implements a generator to generate CWL files.

(define-module (ccwl ccwl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (command
            workflow
            input
            output
            step))

(define-immutable-record-type <input>
  (make-input id type label default binding source other)
  input?
  (id input-id)
  (type input-type)
  (label input-label)
  (default input-default)
  (source input-source set-input-source)
  (other input-other))

(define-immutable-record-type <unspecified-default>
  (make-unspecified-default)
  unspecified-default?)

(define* (input id #:key type label (default (make-unspecified-default)) (other '()))
  "Build and return an <input> object."
  ;; The user should not set source. Hence, do not expose it as a
  ;; parameter of this constructor.
  (let ((source #f))
    (make-input id type label default source other)))

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

(define* (workflow steps outputs #:key (other '()))
(define-immutable-record-type <step>
  (make-step id run in out)
  step?
  (id step-id)
  (run step-run set-step-run)
  (in step-in set-step-in)
  (out step-out set-step-out))

(define (field-appender getter setter)
  (lambda (object element)
    (setter object (cons element (getter object)))))

(define (modify-step-run step proc)
  (set-step-run step (proc (step-run step))))

(define append-step-in
  (field-appender step-in set-step-in))

(define append-step-out
  (field-appender step-out set-step-out))

(define-immutable-record-type <command>
  (make-command inputs outputs args stdin other)
  command?
  (inputs command-inputs set-command-inputs)
  (outputs command-outputs set-command-outputs)
  (args command-args)
  (stdin command-stdin set-command-stdin)
  (other command-other))

(define append-command-inputs
  (field-appender command-inputs set-command-inputs))

(define append-command-outputs
  (field-appender command-outputs set-command-outputs))

(define* (command id arguments #:key (additional-inputs '()) (outputs '()) (other '()))
  (make-step id
             (make-command (append (filter input? arguments)
                                   additional-inputs)
                           outputs arguments #f other)
             (append (filter input? arguments)
                     additional-inputs)
             outputs))

(define (input=? input1 input2)
  (string=? (input-id input1)
            (input-id input2)))

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
(define (output->cwl output)
  `(,(output-id output)
    ,@(filter identity
              (list (and (output-type output)
                         (cons 'type (output-type output)))
                    (and (output-binding output)
                         (cons 'output-binding (output-binding output)))))
    ,@(output-other output)))

(define (command->cwl command)
  `((cwl-version . "v1.1")
    (class . Command-line-tool)
    ,@(command-other command)
    (arguments . ,(list->vector (map (lambda (arg)
                                       (if (input? arg)
                                           (string-append "$(inputs." (input-id arg) ")")
                                           arg))
                                     (command-args command))))
    (inputs . ,(map input->tree (append (command-inputs command)
                                        (if (command-stdin command)
                                            (list (command-stdin command))
                                            (list)))))
    (outputs . ,(map output->cwl (command-outputs command)))
    ,@(if (command-stdin command)
          `((stdin . ,(string-append "$(inputs."
                                     (input-id (command-stdin command))
                                     ".path)")))
          '())))
