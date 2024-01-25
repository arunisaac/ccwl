;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2023–2024 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (ccwl-package)
  #:use-module ((gnu packages bioinformatics) #:prefix guix:)
  #:use-module ((gnu packages emacs) #:select (emacs-minimal))
  #:use-module ((gnu packages fonts) #:select (font-charter font-fira-code))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((guix build-system guile) #:select (%guile-build-system-modules))
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils))

(define-public ccwl
  (package
    (inherit guix:ccwl)
    (source (local-file ".."
                        "ccwl-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))))

(define ccwl-source-with-git-repo
  (local-file ".."
              "ccwl-checkout-with-git-repo"
              #:recursive? #t))

(define ccwl-website-gexp
  (let ((development-profile
         (profile
          (content (package->development-manifest ccwl))
          (allow-collisions? #t))))
    (with-imported-modules %guile-build-system-modules
      #~(begin
          (use-modules (guix build guile-build-system)
                       (guix build utils))

          (set-path-environment-variable
           "PATH" (list "/bin") (list #$development-profile
                                      #$emacs-minimal #$git-minimal))
          (set-path-environment-variable
           "LIBRARY_PATH" (list "/lib") (list #$development-profile))
          (set-path-environment-variable
           "GUILE_LOAD_PATH"
           (list (string-append "/share/guile/site/"
                                (target-guile-effective-version)))
           (list #$development-profile))
          (set-path-environment-variable
           "GUILE_LOAD_COMPILED_PATH"
           (list (string-append "/lib/guile/" (target-guile-effective-version) "/site-ccache"))
           (list #$development-profile))
          (copy-recursively #$ccwl-source-with-git-repo
                            (getcwd))
          ;; Emacs modifies README.org presumably for the contained
          ;; org dynamic block. So, grant write permissions.
          (chmod "README.org" #o644)
          (for-each patch-shebang
                    (list "pre-inst-env"
                          "build-aux/generate-cwl-output.sh"
                          "scripts/ccwl"))
          (substitute* "Makefile"
            (("\\$\\(GUIX_ENVIRONMENT\\)")
             #$(profile
                (content (packages->manifest (list font-charter font-fira-code))))))
          (invoke "sh" "configure")
          (invoke "make"
                  "--jobs" (number->string (parallel-job-count)))
          (invoke "make" "website"
                  "--jobs" (number->string (parallel-job-count)))
          (copy-recursively "website" #$output)))))

(define-public ccwl-website
  (computed-file "ccwl-website" ccwl-website-gexp))

ccwl
