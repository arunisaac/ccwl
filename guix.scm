;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
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

;;; Commentary:

;; This script packages ccwl for GNU Guix.
;; 
;; Run the following command to enter a development environment for
;; ccwl:
;;
;;  $ guix shell -Df guix.scm

;;; Code:

(use-modules (gnu packages autotools)
             (gnu packages bioinformatics)
             (gnu packages graphviz)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages skribilo)
             (gnu packages texinfo)
             (guix build-system gnu)
             (guix gexp)
             (guix git-download)
             (guix packages)
             ((guix licenses) #:prefix license:)
             (guix utils))

(define %source-dir (dirname (current-filename)))

(define ccwl
  (package
    (name "ccwl")
    (version "0.1.0")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0"))) ; to prevent guild warnings
    (inputs
     `(("guile" ,guile-3.0)
       ("guile-libyaml" ,guile-libyaml)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ;; To build documentation
       ("cwltool" ,cwltool)
       ("graphviz" ,graphviz)
       ("skribilo" ,skribilo)))
    (home-page "https://git.systemreboot.net/ccwl")
    (synopsis "Concise common workflow language")
    (description "Concise common workflow language")
    (license license:gpl3+)))

ccwl
