;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2023–2026 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module ((gnu packages skribilo)
                #:select (skribilo) #:prefix guix:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define skribilo
  (package
    (inherit guix:skribilo)
    (name "skribilo")
    (version "0.10.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/skribilo/skribilo-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "03pm2a9a5k0wkj10ywh6xi8flawm8sd396k4698gvvbc2zp4izwc"))))))

(define-public ccwl
  (package
    (inherit guix:ccwl)
    (source (local-file ".."
                        "ccwl-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))
    (native-inputs
     (modify-inputs (package-native-inputs guix:ccwl)
       (replace "skribilo" skribilo)))))

ccwl
