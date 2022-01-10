;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode))
 (makefile-gmake-mode
  (indent-tabs-mode t))
 (scheme-mode
  (eval put 'lambda** 'scheme-indent-function 1)
  (eval put 'syntax-lambda** 'scheme-indent-function 1)
  (eval put 'workflow 'scheme-indent-function 1)))
