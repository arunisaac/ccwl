;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2024, 2026 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (ccwl-distribution)
  #:use-module (ccwl-package)
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((guix build-system guile) #:select (%guile-build-system-modules))
  #:use-module (guix gexp)
  #:use-module (guix profiles))

(define ccwl-git-repo
  (local-file "../.git"
              "ccwl-git-repo"
              #:recursive? #t))

;; ccwl-git-repo cannot find the git repo when built from a Guix
;; channel. So, ccwl-distribution cannot be built from a Guix channel.
(define-public ccwl-distribution-gexp
  (let ((development-profile
         (profile
          (content (package->development-manifest ccwl))
          (allow-collisions? #t))))
    (with-imported-modules %guile-build-system-modules
      #~(begin
          (use-modules (guix build guile-build-system)
                       (guix build utils)
                       (ice-9 ftw)
                       (ice-9 match)
                       (srfi srfi-26))

          (set-path-environment-variable
           "PATH" (list "/bin") (list #$development-profile #$git-minimal))
          (set-path-environment-variable
           "GUILE_LOAD_PATH"
           (list (string-append "/share/guile/site/"
                                (target-guile-effective-version)))
           (list #$development-profile))
          (set-path-environment-variable
           "GUILE_LOAD_COMPILED_PATH"
           (list (string-append "/lib/guile/" (target-guile-effective-version) "/site-ccache"))
           (list #$development-profile))
          (invoke "git" "clone" (string-append "file://" #$ccwl-git-repo) (getcwd))
          (invoke "sh" "configure")
          (invoke "make" "dist")
          (match (scandir (getcwd) (cut string-suffix? ".tar.lz" <>))
            ((tarball)
             (install-file tarball #$output)))))))

(define-public ccwl-distribution
  (computed-file "ccwl-distribution" ccwl-distribution-gexp))

ccwl-distribution
