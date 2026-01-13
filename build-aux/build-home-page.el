;;; ccwl --- Concise Common Workflow Language
;;; Copyright © 2021, 2022, 2024–2026 Arun Isaac <arunisaac@systemreboot.net>
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

;; This Emacs script generates the home page of the website from
;; README.org.

;;; Code:

(require 'ox-html)

(setq make-backup-files nil
      org-export-with-section-numbers nil
      org-export-with-sub-superscripts nil
      org-export-with-toc nil
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-postamble nil)

(defun org-dblock-write:download (params)
  "Dynamically write download block."
  (insert "* Download

Download release tarballs.

")
  (dolist (release '(("2026-01-13" "0.5.0")
                     ("2025-01-28" "0.4.0")
                     ("2024-01-26" "0.3.0")
                     ("2021-11-05" "0.2.0")
                     ("2021-07-06" "0.1.0")))
    (pcase release
      (`(,date ,version)
       (insert (format "- %s [[./releases/ccwl-%s.tar.lz][ccwl-%s.tar.lz]] [[./releases/ccwl-%s.tar.lz.asc][GPG Signature]]\n"
                       date
                       version
                       version
                       version)))))
  (insert "
Download [[https://systemreboot.net/about/arunisaac.pub][public signing key]].

Browse the [[https://github.com/arunisaac/ccwl][development version]] of ccwl hosted on GitHub.")
  ;; Fix tarball filenames.
  (replace-string "ccwl-v" "ccwl-" nil nil nil t))

(defun build-website ()
  (with-current-buffer (find-file "README.org")
    (org-update-all-dblocks)
    ;; Unfortunately, org is unable to handle headlines inside dynamic
    ;; blocks. So, dissolve the download dynamic block.
    (replace-string "#+BEGIN: download" "")
    (replace-string "#+END:" "")
    (org-export-to-file 'html "website/index.html")))
