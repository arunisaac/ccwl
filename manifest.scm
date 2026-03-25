(use-modules ((gnu packages guile-xyz) #:select (guile-ares-rs))
             ((gnu packages task-management) #:select (git-bug))
             ((ccwl-package) #:select (ccwl))
             (srfi srfi-1))

(define (manifest-cons* . args)
  "ARGS is of the form (PACKAGES ... ONTO-MANIFEST). Return a manifest
with PACKAGES and all packages in ONTO-MANIFEST."
  (let ((packages (drop-right args 1))
        (onto-manifest (last args)))
    (manifest (append (map package->manifest-entry packages)
                      (manifest-entries onto-manifest)))))

(manifest-cons* git-bug
                guile-ares-rs
                (package->development-manifest ccwl))
