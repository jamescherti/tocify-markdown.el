;;; tocify-markdown.el --- A simple TOC generator for markdown file  -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2020 Antoine R. Dumont (@ardumont)

;; Author: Antoine R. Dumont (@ardumont)
;; Maintainer: Antoine R. Dumont (@ardumont)
;; URL: http://github.com/ardumont/tocify-markdown
;; Created: 24th May 2014
;; Version: 0.1.5
;; Keywords: markdown, toc, tools,
;; Package-Requires: ((emacs "24.1") (markdown-mode "2.1") (dash "2.11.0") (s "1.9.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Generate a TOC from a markdown file: M-x tocify-markdown-generate-toc
;; This will compute the TOC at insert it at current position.
;; Update existing TOC: C-u M-x tocify-markdown-generate-toc

;; Here is a possible output:
;; <!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
;; **Table of Contents**

;; - [some markdown page title](#some-markdown-page-title)
;; - [main title](#main-title)
;;   - [Sources](#sources)
;;     - [Marmalade (recommended)](#marmalade-recommended)
;;     - [Melpa-stable](#melpa-stable)
;;     - [Melpa (~snapshot)](#melpa-~snapshot)
;;   - [Install](#install)
;;     - [Load org-trello](#load-org-trello)
;;   - [Alternative](#alternative)
;;     - [Git](#git)
;;     - [Tar](#tar)
;; - [another title](#another-title)
;;   - [with](#with)
;;   - [some](#some)
;; - [heading](#heading)
;;
;; <!-- tocify-markdown end -->

;; Install - M-x package-install RET tocify-markdown RET

;;; Code:

(require 's)
(require 'dash)
(require 'markdown-mode)
(require 'find-func) ; find-library-name

(defgroup tocify-markdown nil
  "A simple TOC generator for markdown file."
  :group 'markdown)

(defcustom tocify-markdown-list-item-marker
  "-"
  "List item marker that should be used.
Example: '-' for unordered lists or '1.' for ordered lists."
  :type '(choice
          (string :tag "Unordered list header" "-")
          (string :tag "Ordered list header" "1."))
  :group 'tocify-markdown)

(defcustom tocify-markdown-header-toc-start
  "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->"
  "Beginning delimiter comment."
  :group 'tocify-markdown
  :type 'string)

(defcustom tocify-markdown-header-toc-title
  "**Table of Contents**"
  "Title comment on TOC header."
  :group 'tocify-markdown
  :type 'string)

(defcustom tocify-markdown-header-toc-end
  "<!-- markdown-toc end -->"
  "Ending delimiter comment."
  :group 'tocify-markdown
  :type 'string)

(defcustom tocify-markdown-indentation-space 2
  "Let the user decide the indentation level."
  :group 'tocify-markdown
  :type 'integer)

(defcustom tocify-markdown-user-toc-structure-manipulation-fn
  (lambda (toc-structure) toc-structure)
  "User crafted function to manipulate toc-structure as user sees fit.

The toc-structure has the following form:
\\='((0 . \"some markdown page title\")
  (0 . \"main title\")
  (1 . \"Sources\")
  (2 . \"Marmalade (recommended)\")
  (2 . \"Melpa-stable\")
  (2 . \"Melpa (~snapshot)\")
  (1 . \"Install\")
  (2 . \"Load org-trello\")
  (2 . \"Alternative\")
  (3 . \"Git\")
  (3 . \"Tar\")
  (0 . \"another title\")
  (1 . \"with\")
  (1 . \"some\")
  (1 . \"heading\"))

If the user wanted to remove the first element, it could for
example define the following function:
  (custom-set-variables
    \\='(tocify-markdown-user-toc-structure-manipulation-fn \\='cdr))

Default to identity function (do nothing)."
  :group 'tocify-markdown
  :type 'function)

(defun tocify-markdown-log-msg (args)
  "Log message ARGS."
  (apply #'message (format "tocify-markdown - %s" (car args)) (cdr args)))

(defun tocify-markdown--compute-toc-structure-from-level (level menu-index)
  "Given a LEVEL and a MENU-INDEX, compute the toc structure."
  (when menu-index
    (let* ((fst   (car menu-index))
           (tail  (cdr menu-index))
           (ttail (if (integerp tail) nil (cdr tail))))
      (cons `(,level . ,fst)
            (--mapcat
             (tocify-markdown--compute-toc-structure-from-level (+ 1 level) it)
             ttail)))))

(defun tocify-markdown--compute-toc-structure (imenu-index)
  "Given a IMENU-INDEX, compute the TOC structure."
  (--mapcat (tocify-markdown--compute-toc-structure-from-level 0 it) imenu-index))

(defun tocify-markdown--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
       (-repeat it sym)
       (s-join "" it)))

(defconst tocify-markdown--dash-protection-symbol "09876543214b825dc642cb6eb9a060e54bf8d69288fbee49041234567890"
  "Implementation detail to protect the - characters when converting to link.")

(defconst tocify-markdown--underscore-protection-symbol "afec96cafb7bc4b0e216bfe86db4bd6c4aab44bca19dd9999b11e162f595d711"
  "Implementation detail to protect the `_` characters when converting to link.")

(defun tocify-markdown--to-link (title &optional count)
  "Given a TITLE, return the markdown link associated.
If COUNT is provided and greater than 0, it appends '-COUNT' to the link to
ensure uniqueness."
  (let ((count (if count count 0)))
    (format "[%s](#%s%s)" title
            (->> title
                 s-trim
                 downcase
                 (s-replace "-" tocify-markdown--dash-protection-symbol)
                 (s-replace "_" tocify-markdown--underscore-protection-symbol)
                 (replace-regexp-in-string "[[:punct:]]" "")
                 (s-replace tocify-markdown--dash-protection-symbol "-")
                 (s-replace tocify-markdown--underscore-protection-symbol "_")
                 (s-replace " " "-"))
            (if (> count 0)
                (concat "-" (number-to-string count))
              ""))))

(defun tocify-markdown--count-duplicate-titles (toc-structure)
  "Counts the occurrences of each title in the TOC structure.
Appends the count to each title, with the count starting at 0 for the first
occurrence.
TOC-STRUCTURE is a list of cons cells, where each cons cell contains:
  - The indentation level (an integer).
  - The title of the section (a string)."
  (-map-indexed (lambda (index n)
                  (let* ((indent (car n))
                         (title (cdr n))
                         (count (--count (string= title (cdr it))
                                         (-take (+ index 1) toc-structure))))
                    (list indent title (- count 1))))
                toc-structure))

(defun tocify-markdown--to-tocify-markdown (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, a list of pair level, title, return a TOC string."
  (->> level-title-toc-list
       tocify-markdown--count-duplicate-titles
       (--map (let ((nb-spaces (* tocify-markdown-indentation-space (car it)))
                    (title     (car (cdr it)))
                    (count     (car (cdr (cdr it)))))
                (format "%s%s %s"
                        (tocify-markdown--symbol " " nb-spaces)
                        tocify-markdown-list-item-marker
                        (tocify-markdown--to-link title count))))
       (s-join "\n")))

(defun tocify-markdown--toc-already-present-p ()
  "Determine if a TOC has already been generated.
Return the end position if it exists, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward tocify-markdown-header-toc-start nil t)))


(defun tocify-markdown--toc-start ()
  "Compute the toc's starting point."
  (save-excursion
    (goto-char (tocify-markdown--toc-already-present-p))
    (if (fboundp 'point-at-bol)
        (point-at-bol)
      (line-beginning-position))))

(defun tocify-markdown--toc-end ()
  "Compute the toc's end point."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward tocify-markdown-header-toc-end nil t)))

(defun tocify-markdown--generate-toc (toc-structure)
  "Given a TOC-STRUCTURE, compute a new toc."
  (-> toc-structure
      tocify-markdown--to-tocify-markdown
      tocify-markdown--compute-full-toc))

(defun tocify-markdown--delete-toc (&optional replace-toc-p)
  "Deletes the table of contents (TOC) from the current buffer.

The function identifies the region defined by the TOC's start and end positions,
and deletes it. If REPLACE-TOC-P is non-nil, it moves the point to the start of
the deleted TOC region, effectively replacing it."
  (let ((region-start (tocify-markdown--toc-start))
        (region-end   (tocify-markdown--toc-end)))
    (delete-region region-start (1+ region-end))
    (when replace-toc-p
      (goto-char region-start))))

(defun tocify-markdown--compute-full-toc (toc)
  "Given the TOC's content, compute the full toc with comments and title."
  (format "%s\n%s\n\n%s\n\n%s\n"
          tocify-markdown-header-toc-start
          tocify-markdown-header-toc-title
          toc
          tocify-markdown-header-toc-end))

;;;###autoload
(defun tocify-markdown-generate-toc ()
  "Generate a TOC for markdown file at current point.
Deletes any previous TOC."
  (interactive)
  (save-excursion
    (when (tocify-markdown--toc-already-present-p)
      ;; when toc already present, remove it
      (tocify-markdown--delete-toc t))
    (->> (funcall imenu-create-index-function)
         tocify-markdown--compute-toc-structure
         (funcall tocify-markdown-user-toc-structure-manipulation-fn)
         tocify-markdown--generate-toc
         insert)))

;;;###autoload
(defun tocify-markdown-generate-or-refresh-toc ()
  "Generate a TOC for markdown file at current point or refreshes it."
  (interactive)
  (tocify-markdown-generate-toc))

;;;###autoload
(defun tocify-markdown-refresh-toc ()
  "Refreshes an already generated TOC."
  (interactive)
  (when (tocify-markdown--toc-already-present-p)
    (tocify-markdown-generate-toc)))

;;;###autoload
(defun tocify-markdown-delete-toc ()
  "Deletes a previously generated TOC."
  (interactive)
  (save-excursion
    (tocify-markdown--delete-toc t)))

(defun tocify-markdown--read-title-out-of-link (link)
  "Extract the link title out of a markdown LINK title.
This assumes no funky stuff in the markdown link format ` - [<title>](...) `"
  (->> link
       s-trim
       (s-chop-prefix "- [")
       (s-split "]")
       car))

(defun tocify-markdown--title-level (link)
  "Determine the markdown title LINK out of its indentation.
If misindented or not prefixed by `-`, it's considered not a link
and returns nil. Otherwise, returns the level number."
  (when (s-prefix? "-" (-> link s-trim)) ;; if not, it's not a link title
    (let ((indent (->> link
                       (s-split "-")
                       car  ;; first string contains a string with empty spaces
                       ;; which should be a multiple of
                       ;; `tocify-markdown-indentation-space`
                       length)))
      (when (zerop (% indent tocify-markdown-indentation-space))
        (+ 1 (/ indent tocify-markdown-indentation-space))))))

;;;###autoload
(defun tocify-markdown-follow-link-at-point ()
  "On a given toc link, navigate to the current markdown header.
If the toc is misindented (according to `tocify-markdown-indentation-space')
or if not on a toc link, this does nothing."
  (interactive)
  (let* ((full-title (buffer-substring-no-properties
                      (if (fboundp 'point-at-bol)
                          (point-at-bol)
                        (line-beginning-position))
                      (if (fboundp 'point-at-eol)
                          (point-at-eol)
                        (line-end-position))))
         (level (tocify-markdown--title-level full-title)))
    (if level ;; nil if misindented or not on a title
        (let ((title (tocify-markdown--read-title-out-of-link full-title)))
          (goto-char (point-min))
          (search-forward-regexp (format "%s %s" (s-repeat level "#") title)))
      (message "tocify-markdown: Not on a link (or misindented), nothing to do"))))

(defvar tocify-markdown-mode-map nil "Default Bindings map for tocify-markdown mode.")

(setq tocify-markdown-mode-map
      (let ((map (make-sparse-keymap)))
        map))

;;;###autoload
(define-minor-mode tocify-markdown-mode
  "Functionality for generating toc in markdown file.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{tocify-markdown-mode-map}"

  :init-value nil
  :lighter " TM"
  :group 'tocify-markdown
  :keymap tocify-markdown-mode-map)

(provide 'tocify-markdown)
;;; tocify-markdown.el ends here
