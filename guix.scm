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
             ((gnu packages bioinformatics) #:prefix guix:)
             (guix gexp)
             (guix git-download)
             (guix packages))

(define %source-dir (dirname (current-filename)))

(define ccwl
  (package
    (inherit guix:ccwl)
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ,@(package-native-inputs guix:ccwl)))))

ccwl
