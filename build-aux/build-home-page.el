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

;; This Emacs script generates the home page of the website from
;; README.org.

;;; Code:

(require 'ox-html)

(setq org-export-with-section-numbers nil
      org-export-with-sub-superscripts nil
      org-export-with-toc nil
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-postamble nil)

;; Override org-html--svg-image to export svg using an img tag instead
;; of an object tag.
(defun org-html--svg-image (source attributes info)
  "Return \"object\" embedding svg file SOURCE with given ATTRIBUTES.
INFO is a plist used as a communication channel."
  (let ((attrs (org-html--make-attribute-string
                (org-combine-plists
                 ;; Remove fallback attribute, which is not meant to
                 ;; appear directly in the attributes string, and
                 ;; provide a default class if none is set.
                 '(:class "org-svg") attributes '(:fallback nil)))))
    (org-html-close-tag
     "img" (format "src=\"%s\" %s" source attrs) info)))

(with-current-buffer (find-file "README.org")
  (org-export-to-file 'html "website/index.html"))
