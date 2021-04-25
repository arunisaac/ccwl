;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode))
 (scheme-mode
  (eval put 'set-input-source 'scheme-indent-function nil)
  (eval put 'set-output-source 'scheme-indent-function nil)
  (eval put 'set-step-in 'scheme-indent-function nil)
  (eval put 'lambda** 'scheme-indent-function 1)))
