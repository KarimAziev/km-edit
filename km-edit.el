;;; km-edit.el --- Miscellaneous editing utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-edit
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "28.1") (transient "0.6.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Miscellaneous editing utils

;;; Code:

(require 'subr-x)
(require 'transient)

(declare-function xr-pp-rx-to-str "xr")
(declare-function xr-pp "xr")
(declare-function xr "xr")

(declare-function iedit-find-current-occurrence-overlay "iedit-lib")
(declare-function iedit-barf-if-buffering "iedit-lib")
(declare-function iedit-apply-on-occurrences "iedit-lib")

(defcustom km-edit-truncate-line-modes '(tabulated-list-mode
                                         gh-repo-list-mode
                                         igist-list-mode
                                         osm-mode)
  "List of modes where lines should be truncated instead of wrapped.

A list of major modes where lines should be truncated instead of
wrapped.

Each element in the list should be a symbol representing a major
mode where line truncation is preferred. When the current buffer's
major mode matches one of the modes in this list, lines will be
truncated, and `visual-line-mode' and `visual-fill-column-mode'
will be disabled if they are active."
  :group 'km-edit
  :type '(repeat (symbol :tag "Major Mode")))

(defcustom km-edit-iedit-default-occurence-chars-alist '((org-mode . "_$A-Za-z0-9-"))
  "Alist mapping major modes to default occurrence characters for iedit.

An alist mapping major modes or lists of derived modes to strings
of characters. Each key can be a symbol representing a major mode
or a list of symbols representing derived modes. The value
associated with each key is a string of characters used to set
`iedit-default-occurrence-local'.

See also `km-edit-setup-iedit-default-occurence'."
  :group 'km-edit
  :type '(alist
          :key-type
          (radio :value org-mode
           (symbol
            :tag "Derivered Mode")
           (repeat
            :tag "Derivered Modes"
            :value (org-mode)
            (symbol)))
          :value-type (string :value "_$A-Za-z0-9-")))

;;;###autoload
(defun km-edit-indent-buffer-or-region ()
  "Indent active region or the entire buffer."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning)
                     (region-end))
    (indent-region (point-min)
                   (point-max))))


;;;###autoload
(defun km-edit-yank-pop-forwards (arg)
  "Call `yank-pop' with ARG if the previous command was yank."
  (interactive "p")
  (if (eq last-command 'yank)
      (yank-pop (- arg))
    (yank)))

;;;###autoload
(defun km-edit-delete-line ()
  "Remove line without putting to kill ring."
  (interactive)
  (save-excursion
    (delete-region (point)
                   (line-end-position))))

(defun km-edit-recode-string (str coding-system)
  "Encode and decode string STR to CODING-SYSTEM."
  (decode-coding-string
   (encode-coding-string
    str
    coding-system)
   coding-system))

(defun km-edit-confirm-and-replace-region (beg end replacement)
  "Confirm and replace region with REPLACEMENT text.

Argument REPLACEMENT is the value that will replace the region.
It should be a string, or an unary function that string or nil.

Argument END is the end position of the region to be replaced.
Argument BEG is the beginning position of the region to be replaced."
  (when-let* ((overlay (make-overlay beg end))
             (rep (if (functionp replacement)
                      (funcall replacement)
                    replacement)))
    (when (unwind-protect
              (progn (overlay-put overlay 'face 'error)
                     (overlay-put overlay 'after-string
                                  (concat
                                   "\s"
                                   (propertize rep
                                               'face 'success)))
                     (yes-or-no-p "Replace region?"))
            (delete-overlay overlay))
      (when (fboundp 'replace-region-contents)
        (replace-region-contents beg end (lambda () rep)))
      rep)))

;;;###autoload
(defun km-edit-recode-buffer-or-region (coding-system)
  "Encode and decode active region or whole buffer content to CODING-SYSTEM."
  (interactive (list (read-coding-system "Coding system:\s" 'utf-8)))
  (pcase-let* ((`(,beg . ,end)
                (if (and
                     (region-active-p)
                     (use-region-p))
                    (cons (region-beginning)
                          (region-end))
                  (cons (point-min)
                        (point-max))))
               (pos (point))
               (text (km-edit-recode-string
                      (buffer-substring-no-properties beg
                                                      end)
                      coding-system)))
    (if (fboundp 'replace-region-contents)
        (replace-region-contents beg end
                                 (lambda () text))
      (goto-char beg)
      (delete-region beg
                     end)
      (insert text)
      (goto-char pos))))

;;;###autoload
(defun km-edit-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (if (minibufferp)
      (beginning-of-line)
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line)))))

(defvar-local km-edit--quote-or-unquote-last-rep nil)

;;;###autoload
(defun km-edit-quote-or-unquote-at-point ()
  "Add or remove string quotes for thing at point."
  (interactive)
  (unless (eq last-command 'km-edit-quote-or-unquote-at-point)
    (setq km-edit--quote-or-unquote-last-rep nil))
  (if-let* ((stx (seq-find (apply-partially #'nth 3)
                           (list (syntax-ppss (point))
                                 (syntax-ppss (1+ (point))))))
            (str-char (nth 3 stx))
            (beg (nth 8 stx))
            (end (save-excursion
                   (goto-char beg)
                   (forward-sexp 1)
                   (point)))
            (content (buffer-substring-no-properties
                      beg end)))
      (let ((rep (substring-no-properties content 1 (1- (length content)))))
        (replace-region-contents beg end (lambda () rep))
        (setq km-edit--quote-or-unquote-last-rep (list
                                                  content
                                                  beg
                                                  (+ beg
                                                     (length rep)))))
    (when-let* ((bounds (if km-edit--quote-or-unquote-last-rep
                            (cons (nth 1 km-edit--quote-or-unquote-last-rep)
                                  (nth 2 km-edit--quote-or-unquote-last-rep))
                          (bounds-of-thing-at-point 'symbol)))
                (item (or (car km-edit--quote-or-unquote-last-rep)
                          (prin1-to-string
                           (buffer-substring-no-properties (car bounds)
                                                           (cdr bounds))))))
      (replace-region-contents (car bounds)
                               (cdr bounds)
                               (lambda () item)))))

(defun km-edit-infer-indentation-style ()
  "Set `indent-tabs-mode' based on the predominant indentation style in the buffer.

Usage example:

\\=(add-hook \\='prog-mode-hook \\='km-edit-infer-indentation-style)."
  (let ((space-count (how-many "^  " (point-min)
                               (point-max)))
        (tab-count (how-many "^\t" (point-min)
                             (point-max))))
    (if (> space-count tab-count)
        (setq indent-tabs-mode nil))
    (if (> tab-count space-count)
        (setq indent-tabs-mode t))))

(defcustom km-edit-dwim-kbd-forms '(define-key local-set-key
                                               global-set-key)
  "List of allowed parent symbols to insert keys wrapped in kbd call.
See `km-edit-dwim-insert-key-description'."
  :type '(repeat symbol)
  :group 'km-edit)


;;;###autoload
(defun km-edit-dwim-insert-key-description ()
  "Read and insert a description of keystrokes.
Inside string also remove old content.
If parent form is a symbol listed in `km-edit-dwim-kbd-forms'
and point is not inside string,
insert either vector or kbd call.
In other cases insert string."
  (interactive)
  (let* ((key (read-key-sequence "Key:\s" nil t))
         (inside-str (nth 3 (syntax-ppss (point))))
         (str-start (when inside-str
                      (nth 8 (syntax-ppss (point)))))
         (inside-define-key
          (save-excursion
            (when str-start
              (goto-char str-start))
            (let ((pos (point)))
              (ignore-errors
                (backward-up-list)
                (unless (equal pos (point))
                  (memq (car-safe
                         (sexp-at-point))
                        km-edit-dwim-kbd-forms))))))
         (descr (cond ((and (vectorp key)
                            inside-define-key
                            (not inside-str))
                       key)
                      (t (key-description key))))
         (result (cond ((and inside-str)
                        (prin1-to-string descr))
                       ((and inside-define-key
                             (not (vectorp descr)))
                        (prin1-to-string `(kbd ,descr)))
                       (t
                        (prin1-to-string descr)))))
    (when str-start
      (while (nth 3 (syntax-ppss (point)))
        (forward-char 1))
      (delete-region str-start (point)))
    (insert result)
    (forward-char -1)))

;;;###autoload
(defun km-edit-copy-sexp-or-region-at-point ()
  "Copy region or sexp at point."
  (interactive)
  (when-let* ((bounds (bounds-of-thing-at-point 'sexp)))
    (km-edit--kill-new (buffer-substring-no-properties
                        (car bounds)
                        (cdr bounds)))
    (buffer-substring-no-properties
     (car bounds)
     (cdr bounds))))

(defun km-edit-get-region ()
  "Return current active region as string or nil."
  (when (and (region-active-p)
             (use-region-p))
    (buffer-substring-no-properties
     (region-beginning)
     (region-end))))

(defun km-edit--kill-new (str)
  "Copy STR to the kill ring and display a message.

Argument STR is the string to be added to the kill ring."
  (kill-new str)
  (message "Copied")
  str)

(defun km-edit--has-region-or-sexp ()
  "Check if a region is active or return bounds of the nearest s-expression."
  (or (and (region-active-p)
           (use-region-p))
      (bounds-of-thing-at-point 'sexp)))

;;;###autoload
(defun km-edit-copy-prin1-to-string-at-point ()
  "Return a string with the printed representation of active region or sexp."
  (interactive)
  (pcase-let* ((`(,beg . ,end)
                (if (and (region-active-p)
                         (use-region-p))
                    (cons (region-beginning)
                          (region-end))
                  (bounds-of-thing-at-point 'sexp)))
               (content (and beg end (buffer-substring-no-properties beg end))))
    (km-edit--kill-new (prin1-to-string content))))

;;;###autoload
(defun km-edit-copy-prin1-to-string-no-newlines ()
  "Return a string with the printed representation of region without new lines."
  (interactive)
  (require 'subr-x)
  (when-let* ((content (string-join
                       (split-string
                        (km-edit-copy-prin1-to-string-at-point)
                        (if
                            (yes-or-no-p "Remove multi spaces?")
                            nil "\n")
                        t)
                       "\s")))
    (if (called-interactively-p 'any)
        (km-edit--kill-new (prin1-to-string content))
      (prin1-to-string content))))

;;;###autoload
(defun km-edit-recode-region (beg end)
  "Try all possible character encodings to re-decode region between BEG and END.
Show results in minibuffer completions and finally recode the region with
selected one."
  (interactive "r")
  (let ((inhibit-read-only t))
    (let* ((str (buffer-substring-no-properties beg
                                                end))
           (str (car (split-string str "[\n]+" t)))
           (codings (find-coding-systems-string str))
           (alist (mapcan (lambda (new-coding)
                            (mapcar (lambda (coding)
                                      (with-temp-buffer
                                        (erase-buffer)
                                        (insert
                                         (substring-no-properties str))
                                        (recode-region (point-min)
                                                       (point-max)
                                                       new-coding coding)
                                        (cons (substring-no-properties
                                               (buffer-string))
                                              (cons new-coding coding))))
                                    codings))
                          coding-system-list))
           (annot-fn (lambda (str)
                       (or (pcase-let ((`(,new-coding ,old-coding)
                                        (cdr (assoc-string str alist))))
                             (format (propertize
                                      (concat
                                       (propertize " " 'display
                                                   '(space
                                                     :align-to
                                                     40))
                                       " %s (from %s)")
                                      'face 'completions-annotations)
                                     new-coding old-coding)))))
           (cell (cdr (assoc (completing-read "Select best result: "
                                              (lambda
                                                (str
                                                 pred
                                                 action)
                                                (if
                                                    (eq
                                                     action
                                                     'metadata)
                                                    `(metadata
                                                      (annotation-function
                                                       .
                                                       ,annot-fn))
                                                  (complete-with-action
                                                   action
                                                   alist
                                                   str
                                                   pred)))
                                              nil
                                              t)
                             alist))))
      (recode-region beg end (car cell)
                     (cdr cell))
      (message
       "Re-decoded by NEW-CODING %s (previously decoded by CODING) %s"
       (car cell)
       (cdr cell)))))

(defun km-edit-org-elisp-to-doc-str (str)
  "Convert description STR in `org-mode' format to elisp documentation string."
  (with-temp-buffer
    (insert str)
    (while (re-search-backward "[=~]\\([^=~]+\\)[=~]" nil t 1)
      (let ((value (match-string-no-properties 1)))
        (replace-match (upcase value))))
    (buffer-string)))

(defun km-edit-elisp-to-org-doc (str)
  "Convert description STR from elisp documentation string to `org-mode'."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "[`']\\([^`']+\\)[`']" nil t 1)
      (let ((value (match-string-no-properties 1))
            (beg (match-beginning 0))
            (end (match-end 0)))
        (delete-region beg end)
        (insert (concat "=" value "="))))
    (buffer-string)))

;;;###autoload
(defun km-edit-copy-elisp-as-org (beg end)
  "Copy and convert Elisp docstring as org.

Argument BEG is the beginning position of the region to copy.

Argument END is the ending position of the region to copy.

Argument STR is the string to convert from `org-mode' format to an Emacs Lisp
documentation string."
  (interactive "r")
  (when (and (region-active-p)
             (use-region-p))
    (let* ((text (mapconcat
                  (apply-partially #'replace-regexp-in-string "^\\([\s;]+\\)")
                  (split-string (buffer-substring-no-properties beg end) "\n")
                  "\n"))
           (rep
            (km-edit-elisp-to-org-doc
             text)))
      (km-edit--kill-new rep)
      (message "Copied:\n%s" rep)
      rep)))

;;;###autoload
(defun km-edit-copy-org-as-elisp-doc (beg end)
  "Copy and convert Org text to Elisp docstring.

Argument BEG is the beginning position of the region to copy.

Argument END is the ending position of the region to copy.

Argument STR is the string to convert from `org-mode' format to an Emacs Lisp
documentation string."
  (interactive "r")
  (when (and (region-active-p)
             (use-region-p))
    (let ((rep
           (km-edit-org-elisp-to-doc-str
            (buffer-substring-no-properties beg end))))
      (km-edit--kill-new rep)
      (message "Copied:\n%s" rep)
      rep)))



(defun km-edit--sexp-to-doc-example (str)
  "Escape open parentheses and unescaped single and double quotes in STR."
  (with-temp-buffer
    (insert (replace-regexp-in-string
             "^\"\\|\"$" ""
             (let (print-length print-level
                                print-circle)
               (prin1-to-string str))))
    (while (re-search-backward
            "\\(^[(']\\|\\(\\([']\\)\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)\\([\s)]\\)\\)\\)"
            nil t 1)
      (if (looking-back "=" 0)
          (forward-char -1)
        (insert "=")
        (forward-char -1))
      (pcase (skip-chars-backward "\\\\")
        (0 (insert "\\\\"))
        (-1 (insert "\\"))))
    (buffer-string)))


;;;###autoload
(defun km-edit-copy-sexp-as-elisp-doc-example (beg end)
  "Copy region as Elisp documentation string.

Argument BEG is the position of the beginning of the region.

Argument END is the position of the end of the region.

Argument STR is a string to be escaped for use in documentation strings."
  (interactive "r")
  (when (and (region-active-p)
             (use-region-p))
    (let ((rep
           (km-edit--sexp-to-doc-example
            (buffer-substring-no-properties beg end))))
      (km-edit--kill-new rep))))



;;;###autoload
(defun km-edit-unqote-region (beg end)
  "Replace region between BEG and END to non string."
  (interactive "r")
  (let* ((reg (buffer-substring-no-properties
               beg
               end))
         (rep
          (when (and (string-prefix-p "\"" reg)
                     (string-suffix-p "\"" reg))
            (replace-regexp-in-string "\\\\\"" "\""
                                      (buffer-substring-no-properties
                                       (1+
                                        beg)
                                       (1-
                                        end))))))
    (replace-region-contents beg end (lambda () rep))))


(defun km-edit--get-xr-to-rx-sexp ()
  "Convert regex string to `rx' sexp at point."
  (let* ((pos (point))
         (stx (syntax-ppss pos)))
    (let ((start
           (cond ((and (nth 3 stx)
                       (nth 8 stx))
                  (nth 8 stx))
                 ((progn (setq stx (ignore-errors (syntax-ppss (1+ pos))))
                         (when (nth 3 stx)
                           (nth 8 stx)))
                  (nth 8 stx))
                 ((progn
                    (setq stx (ignore-errors
                                (syntax-ppss (1- pos))))
                    (when (nth 3 stx)
                      (nth 8 stx)))
                  (nth 8 stx))))
          (regex))
      (when start (save-excursion
                    (goto-char start)
                    (setq regex (sexp-at-point))))
      (when regex
        (concat "(rx "
                (xr-pp-rx-to-str (xr
                                  regex
                                  nil))
                ")")))))
;;;###autoload
(defun km-edit-xr-to-rx-at-point ()
  "Convert a regular expression at point from XR to RX format.
Requires xr lib."
  (interactive)
  (require 'xr)
  (let* ((pos (point))
         (stx (syntax-ppss pos)))
    (let ((start
           (cond ((and (nth 3 stx)
                       (nth 8 stx))
                  (nth 8 stx))
                 ((progn (setq stx (syntax-ppss (1+ pos)))
                         (when (nth 3 stx)
                           (nth 8 stx)))
                  (nth 8 stx))
                 ((progn
                    (setq stx (syntax-ppss (1- pos)))
                    (when (nth 3 stx)
                      (nth 8 stx)))
                  (nth 8 stx))))
          (end)
          (regex))
      (when start (save-excursion
                    (goto-char start)
                    (setq regex (sexp-at-point))
                    (forward-sexp 1)
                    (setq end (point))))
      (when regex
        (replace-region-contents start end
                                 (lambda ()
                                   (concat "(rx "
                                           (xr-pp-rx-to-str (xr
                                                             regex
                                                             nil))
                                           ")")))))))

;;;###autoload
(defun km-edit-copy-regex-at-point-as-rx ()
  "Copy regex at point to kill ring as `rx' syntax."
  (interactive)
  (require 'xr)
  (when-let* ((regex (km-edit--get-xr-to-rx-sexp)))
    (km-edit--kill-new regex)
    (message "copied %s" regex)
    regex))


(defun km-edit--split-string-replacement ()
  "Split and reformat a string from the buffer or region."
  (pcase-let*
      ((`(,start . ,end)
        (if (and
             (region-active-p)
             (use-region-p))
            (cons (region-beginning)
                  (region-end))
          (save-excursion
            (when-let* ((str-start (nth 8 (syntax-ppss (point)))))
              (goto-char str-start)
              (when (looking-at "\"")
                (cons (point)
                      (save-excursion
                        (forward-sexp 1)
                        (point))))))))
       (reg (and start end
                 (buffer-substring-no-properties
                  start end))))
    (when reg
      (list start end
            (mapconcat #'prin1-to-string
                       (split-string
                        (if (string-match-p "^\""
                                            reg)
                            (condition-case  nil
                                (car-safe (read-from-string
                                           reg))
                              (error reg))
                          reg))
                       "\s")))))

;;;###autoload
(defun km-edit-split-string ()
  "Split active region to strings.
For example, inside string:

\"add html head extra\" => \"add\" \"html\" \"head\" \"extra\"

a b => \"a\" \"b\"."
  (interactive)
  (pcase-let ((`(,start ,end ,replacement)
               (km-edit--split-string-replacement)))
    (when replacement
      (when-let* ((overlay (make-overlay start end)))
        (when (unwind-protect
                  (progn (overlay-put overlay 'face 'error)
                         (overlay-put overlay 'after-string
                                      (concat
                                       "\s"
                                       (propertize replacement
                                                   'face 'success)))
                         (yes-or-no-p "Replace region?"))
                (delete-overlay overlay))
          (when (fboundp 'replace-region-contents)
            (replace-region-contents start end (lambda () replacement))))))))

;;;###autoload
(defun km-edit-query-replace-regex ()
  "Replace a symbol at point using `kill-ring' defaults."
  (interactive)
  (let* ((from-string (read-string "Replace: "
                                   (or
                                    (km-edit-get-region)
                                    (thing-at-point 'symbol))))
         (chars (seq-remove
                 (lambda (c)
                   (string-match-p "[a-z0-9]" c))
                 (split-string
                  from-string
                  ""
                  t)))
         (replacements (delete-dups
                        (seq-filter
                         (lambda (str)
                           (and
                            (or (not chars)
                                (seq-find (lambda (s)
                                            (member s chars))
                                          (split-string str "" t)))
                            (not (string= str from-string))
                            (string-match-p
                             "[a-z]" str)
                            (not (string-match-p
                                  "[\s\t\n\r\f){(}\"']"
                                  str))))
                         kill-ring)))
         (to-string (completing-read (format "Replace %s with: " from-string)
                                     replacements
                                     nil nil)))
    (query-replace from-string to-string
                   nil
                   (point-min)
                   (point-max))))

(defun km-edit-remove-spaces-between-empty-lines ()
  "Remove extra spaces between empty lines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\\([\s]+\\)\n" nil t 1)
      (replace-match "" nil nil nil 1))))

;;;###autoload
(define-minor-mode km-edit-whitespace-cleanup-mode
  "Activate to clean whitespace on save.

Activate `km-edit-whitespace-cleanup-mode' to automatically remove extra spaces
between empty lines before saving a buffer.

When enabled, this mode ensures that any sequence of spaces on lines by
themselves are deleted upon saving, maintaining a cleaner file structure. Toggle
off this mode to disable this automatic cleanup behavior."
  :lighter " WS-clean"
  :global nil
  (if km-edit-whitespace-cleanup-mode
      (add-hook 'before-save-hook #'km-edit-remove-spaces-between-empty-lines
                nil 'local)
    (remove-hook 'before-save-hook #'km-edit-remove-spaces-between-empty-lines
                 'local)))

;;;###autoload
(defun km-edit-markdown-quotes-to-org-quotes (replacement)
  "Convert Markdown quotes to Org-mode quotes in text.

Argument REPLACEMENT is the string to replace matched quotes with. It
defaults to prompting the user with a completion list."
  (interactive (list
                (completing-read "Replace with: " '("=" "~"))))
  (pcase-let ((`(,beg . ,end)
               (if (region-active-p)
                   (car (region-bounds))
                 (cons (point-min)
                       (point-max)))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\([`]\\)[a-z0-9._-]+\\(['`]\\)" end t 1)
        (replace-match replacement nil nil nil 1)
        (replace-match replacement nil nil nil 2)))))

(defun km-edit--make-toggled-description (mode &optional description align)
  "Concat DESCRIPTION for MODE with colorized suffixes ON-LABEL and OFF-LABEL."
  (concat
   (propertize
    (or
     description
     (when-let* ((doc (replace-regexp-in-string
                      "-" " " (capitalize (symbol-name
                                           mode)))))
       (replace-regexp-in-string "\\.$" ""
                                 (car
                                  (split-string doc "\n" nil)))))
    'face
    (if
        (and (boundp mode)
             (symbol-value mode))
        'success nil))
   (propertize " " 'display
               (list 'space :align-to (or align 50)))
   (if (and (boundp mode)
            (symbol-value mode))
       "[X]" "[ ]")))

;;;###autoload (autoload 'km-edit-recode-menu "km-edit" nil t)
(transient-define-prefix km-edit-recode-menu ()
  "Display recoding options for text selection."
  [("k"
    "Recode (show all variants)"
    km-edit-recode-region)
   ("b"
    "Encode and decode"
    km-edit-recode-buffer-or-region)])

;;;###autoload (autoload 'km-edit-menu "km-edit" nil t)
(transient-define-prefix km-edit-menu ()
  "A transient menu for various editing operations."
  :transient-suffix     nil
  :transient-non-suffix nil
  [["Copy"
    ("s" "elisp => doc string" km-edit-copy-sexp-as-elisp-doc-example
     :inapt-if-not km-edit-get-region)
    (";" "elisp => org" km-edit-copy-elisp-as-org :inapt-if-not
     km-edit-get-region)
    ("d" "org => elisp" km-edit-copy-org-as-elisp-doc :inapt-if-not
     km-edit-get-region)
    ("n" "as prin1 to string" km-edit-copy-prin1-to-string-at-point
     :inapt-if-not km-edit--has-region-or-sexp)
    ("N" "as prin1 to string one line"
     km-edit-copy-prin1-to-string-no-newlines
     :inapt-if-not km-edit--has-region-or-sexp)
    ("a" "sexp at point"
     km-edit-copy-sexp-or-region-at-point
     :inapt-if-not (lambda ()
                     (bounds-of-thing-at-point 'sexp)))]
   ["Replace"
    ("." "query replace" km-edit-query-replace-regex)
    ("o" "markdown => org" km-edit-markdown-quotes-to-org-quotes
     :inapt-if-not (lambda ()
                     (save-excursion
                       (pcase-let ((`(,beg . ,end)
                                    (if (region-active-p)
                                        (car (region-bounds))
                                      (cons (point-min)
                                            (point-max)))))
                         (save-excursion
                           (goto-char beg)
                           (re-search-forward "\\([`]\\)[a-z0-9._-]+\\(['`]\\)"
                                              end t 1))))))
    ""
    ("u" "unquote region" km-edit-unqote-region
     :inapt-if-not (lambda ()
                     (when-let* ((reg (km-edit-get-region)))
                       (and (string-prefix-p "\"" reg)
                            (string-suffix-p "\"" reg)))))
    ("e" km-edit-split-string
     :inapt-if-not km-edit--split-string-replacement
     :description
     (lambda ()
       (or (car (last (km-edit--split-string-replacement)))
           "region to strings")))
    "Regex"
    ("r" "Copy xr => rx" km-edit-copy-regex-at-point-as-rx
     :inapt-if-not (lambda ()
                     (save-excursion
                       (km-edit--get-xr-to-rx-sexp))))
    ("p" "Convert xr => rx" km-edit-xr-to-rx-at-point
     :inapt-if-not (lambda ()
                     (save-excursion
                       (km-edit--get-xr-to-rx-sexp))))]]
  [[("m" km-edit-whitespace-cleanup-mode
     :description (lambda ()
                    (km-edit--make-toggled-description
                     'km-edit-whitespace-cleanup-mode))
     :transient t)
    ("E" "Encode or decode" km-edit-recode-menu)]])

;;;###autoload (autoload 'km-edit-multi-cursors-menu "km-edit" nil t)
(transient-define-prefix km-edit-multi-cursors-menu ()
  "Command dispatcher for `multiple-cursors'."
  :transient-suffix #'transient--do-call
  :transient-non-suffix #'transient--do-exit
  [:if (lambda ()
         (require 'multiple-cursors nil t))
   ("l" "Add one cursor to each line of the active region"
    mc/edit-lines
    :transient nil
    :inapt-if-not use-region-p)
   ("a"
    "Find and mark all the parts of the buffer matching the currently active region"
    mc/mark-all-like-this
    :transient nil)
   ("n"
    mc/mark-next-like-this
    :description (lambda ()
                   (concat "Add cursor to " (if (region-active-p)
                                                " region lines"
                                              " next line"))))
   ("N"
    "Skip next line"
    mc/skip-to-next-like-this)
   ("M-n"
    "Deselect next part of the buffer matching the currently active region"
    mc/unmark-next-like-this)
   ("p"
    "Mark previous part of the buffer matching the currently active region"
    mc/mark-previous-like-this)
   ("S"
    "Skip the current one and select the prev part of the buffer"
    mc/skip-to-previous-like-this)
   ("M-p"
    "Deselect prev part of the buffer matching the currently active region"
    mc/unmark-previous-like-this)
   ("s"
    "Find and mark all the parts in the region matching the given regexp"
    mc/mark-all-in-region-regexp
    :transient nil)]
  [:if (lambda ()
         (require 'multiple-cursors nil t))
   :description "Insert"
   [("0" "Insert increasing numbers from 0" mc/insert-numbers
     :transient nil)
    ("A" "Insert increasing letters for each cursor"
     mc/insert-letters
     :transient nil)]])

;;;###autoload (autoload 'km-edit-string-inflection-menu "km-edit" nil t)
(transient-define-prefix km-edit-string-inflection-menu ()
  :transient-suffix #'transient--do-call
  [:if (lambda ()
         (require 'string-inflection nil t))
   ("."
    "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
    string-inflection-all-cycle)
   ("t" "foo_bar <=> FooBar" string-inflection-toggle)
   ("-" "foo-bar" string-inflection-kebab-case)
   ("l" "fooBar" string-inflection-lower-camelcase)
   ("_" "foo_bar" string-inflection-underscore)
   ("u" "FOO_BAR" string-inflection-upcase)])

(defun km-edit-maybe-disable-truncate-lines ()
  "Disable `truncate-lines' if `visual-line-mode' is enabled."
  (cond ((bound-and-true-p visual-line-mode)
         (setq truncate-lines nil))))

(defun km-edit-setup-visual-line-mode ()
  "Set up line truncation or visual line mode based on the current major mode."
  (cond ((derived-mode-p km-edit-truncate-line-modes)
         (setq truncate-lines t)
         (when (bound-and-true-p visual-line-mode)
           (visual-line-mode -1))
         (when (and (bound-and-true-p visual-fill-column-mode)
                    (fboundp 'visual-fill-column-mode))
           (visual-fill-column-mode -1)))
        ((derived-mode-p 'comint-mode)
         (setq truncate-lines nil))
        (t
         (visual-line-mode 1))))

;;;###autoload
(defun km-edit-format-slack-messages ()
  "Format selected Slack messages into a quoted block."
  (interactive)
  (let ((rbeg (or (and (region-active-p)
                       (use-region-p)
                       (region-beginning))
                  (point-min)))
        (rend (or (region-end)
                  (point-max)))
        (re "\\(^[a-z\s]+\\)\n[\s]*[0-9][0-9]?:[0-9][0-9]\\([\s][PA][M]\\)$"))
    (goto-char rbeg)
    (let ((results)
          (done))
      (while
          (when (and (not done)
                     (> rend (point)))
            (re-search-forward
             re
             rend
             t 1))
        (let* ((author (match-string-no-properties 1))
               (beg (point))
               (end
                (when (re-search-forward
                       re
                       nil rend 1)
                  (goto-char (match-beginning 0))
                  (point)))
               (msg (buffer-substring-no-properties beg (or end rend))))
          (unless end
            (setq done t))
          (setq msg (replace-regexp-in-string "^[0-9][0-9]?:[0-9][0-9]$" "" msg))
          (setq msg (replace-regexp-in-string "^:[_a-z0-9+-]+:\n[0-9]+" "" msg))
          (setq msg (replace-regexp-in-string "^:[_a-z0-9+-]+:\n[0-9]+" "" msg))
          (setq msg (replace-regexp-in-string "^[\n][\n]+" "\n" msg))
          (setq msg (format "*%s*:\n#+begin_quote\n%s\n#+end_quote" (string-trim author) (string-trim msg)))
          (setq results (push msg results))))
      (setq results (string-join (nreverse results) "\n\n"))
      (delete-region rbeg rend)
      (insert results))))

(defvar iedit-case-sensitive)
(defvar iedit-mode-occurrence-keymap)
(defvar iedit-default-occurrence-local)
(defvar iedit-occurrence-type-local)

(defvar km-edit--minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") #'km-edit-multi-source-select-next)
    (define-key map (kbd "C-<") #'km-edit-multi-source-select-prev)
    (define-key map (kbd "C-.") #'km-edit-multi-source-read-source)
    map)
  "Keymap to use in minibuffer.")

(defun km-edit-multi-source-select-next ()
  "Throw to the catch tag ='next with 1."
  (interactive)
  (throw 'next
         1))

(defun km-edit-multi-source-select-prev ()
  "Navigate to the previous item in a multi-source selection."
  (interactive)
  (throw 'next
         -1))


(defun km-edit-multi-source-read-source ()
  "Prompt user to select a source and calculate index offset."
  (interactive)
  (throw 'next t))

(defun km-edit--index-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))
        (t current-index)))



(defun km-edit-iedit-read-occurence (prompt items)
  "Read a string from the minibuffer with optional completion and default values.

Argument PROMPT is a string used to prompt the user for input.

Argument ITEMS is a list of strings representing the possible choices."
  (let ((curr-idx 0)
        (curr)
        (reader 'read-string))
    (while
        (let ((item (catch 'next
                      (minibuffer-with-setup-hook
                          (lambda ()
                            (use-local-map
                             (let ((map
                                    (copy-keymap
                                     km-edit--minibuffer-map)))
                               (set-keymap-parent map (current-local-map))
                               map)))
                        (let ((window-selection-change-functions nil))
                          (let* ((default-rep (nth curr-idx items))
                                 (prompt-with-def
                                  (concat
                                   prompt
                                   (unless (string-suffix-p " "
                                                            prompt)
                                     " ")
                                   (when default-rep
                                     (format "(default %s) "
                                             (propertize
                                              (substring-no-properties
                                               default-rep)
                                              'face
                                              'font-lock-function-name-face))))))
                            (pcase reader
                              ('completing-read (completing-read
                                                 prompt-with-def
                                                 items))
                              (_ (read-string
                                  (concat
                                   (when items
                                     (format "(%d/%d) " (1+ (or curr-idx
                                                                0))
                                             (length items)))
                                   prompt-with-def)
                                  nil nil
                                  default-rep
                                  nil)))))))))
          (cond ((numberp item)
                 (setq reader 'read-string)
                 (setq curr-idx (km-edit--index-switcher item curr-idx items)))
                ((eq item t)
                 (setq reader 'completing-read))
                ((stringp item)
                 (setq curr item)
                 nil))))
    curr))

(defvar iedit-occurrences-overlays)
;;;###autoload
(defun km-edit-iedit-replace-occurrences (&optional to-string)
  "Replace all occurrences of a string with another string interactively.

Optional argument TO-STRING is the string to replace occurrences with."
  (interactive "*")
  (require 'iedit)
  (iedit-barf-if-buffering)
  (let* ((ov (iedit-find-current-occurrence-overlay))
         (offset (- (point)
                    (overlay-start ov)))
         (from-string (buffer-substring-no-properties
                       (overlay-start ov)
                       (overlay-end ov)))
         (to-string (if (not to-string)
                        (let ((choices
                               (delete-dups
                                (seq-drop-while (apply-partially #'equal
                                                                 from-string)
                                                kill-ring))))
                          (unless (string-match-p "[\n]" from-string)
                            (setq choices (seq-remove (apply-partially #'string-match-p "\n") choices)))
                          (unless (string-match-p "[\s\t]" from-string)
                            (setq choices (seq-remove (apply-partially #'string-match-p "[\s\t]") choices)))
                          (km-edit-iedit-read-occurence
                           (concat
                            (format "Replace %d "
                                    (length
                                     iedit-occurrences-overlays))
                            (if (= 1 (length iedit-occurrences-overlays))
                                "occurence"
                              "occurences")
                            " of " (propertize
                                    (truncate-string-to-width
                                     (substring-no-properties
                                      from-string
                                      0
                                      (string-match-p
                                       "\n" from-string))
                                     70
                                     nil
                                     nil
                                     t)
                                    'face
                                    'font-lock-function-name-face)
                            " with: ")
                           choices))
                      to-string)))
    (iedit-apply-on-occurrences
     (lambda (beg end from-string to-string)
       (goto-char beg)
       (search-forward from-string end)
       (replace-match to-string (not (and (not iedit-case-sensitive)
                                          case-replace))))
     from-string to-string)
    (goto-char (+ (overlay-start ov) offset))))

(defun km-edit--format-keymap (sym &optional full shadow prefix title with-menu
                                   transl always-title mention-shadow buffer)
  "Substitute keymap symbols with their values in a temporary buffer.

Argument SYM is a keymap, a string naming a symbol, or a symbol whose value is a
keymap.

Optional argument FULL is a boolean; when non-nil, it includes inherited
keymaps.

Optional argument SHADOW is a boolean; when non-nil, it shows keys that are
shadowed by higher-precedence maps.

Optional argument PREFIX is a string or a vector of events to be used as a
PREFIX for keys in the keymap.

Optional argument TITLE is a string used as the title of the keymap description.

Optional argument WITH-MENU is a boolean; when non-nil, it includes menu
bindings.

Optional argument TRANSL is a boolean; when non-nil, it translates keys to their
ASCII equivalents.

Optional argument ALWAYS-TITLE is a boolean; when non-nil, it forces the display
of the TITLE even if it would normally be omitted.

Optional argument MENTION-SHADOW is a boolean; when non-nil, it mentions when a
binding is shadowed by another map.

Optional argument BUFFER is the buffer in which the keymap is active; defaults
to the current buffer."
  (when-let* ((value
              (cond ((keymapp sym)
                     sym)
                    ((stringp sym)
                     (ignore-errors (symbol-value (intern-soft sym))))
                    ((symbolp sym)
                     (symbol-value sym))))
             (buff (or buffer (current-buffer))))
    (with-temp-buffer
      (funcall (with-no-warnings
                 (if (fboundp 'help--describe-map-tree)
                     #'help--describe-map-tree
                   #'describe-map-tree))
               value
               (not full)
               shadow
               prefix
               title
               (not with-menu)
               transl
               always-title
               mention-shadow
               buff)
      (buffer-string))))

;;;###autoload
(defun km-edit-iedit-show-keymap ()
  "Display key bindings in `iedit-mode' using `lv-message'."
  (interactive)
  (require 'lv nil t)
  (require 'iedit)
  (when (and (bound-and-true-p iedit-mode)
             (fboundp 'lv-delete-window))
    (lv-delete-window)
    (when-let* ((msg
                (string-join (seq-remove
                              (lambda (str)
                                (or (string-empty-p str)
                                    (string-prefix-p "<remap" str)))
                              (split-string (km-edit--format-keymap
                                             iedit-mode-occurrence-keymap
                                             t nil nil nil nil nil
                                             nil nil
                                             (current-buffer))
                                            "\n" t))
                             "\n")))
      (when (fboundp 'lv-message)
        (add-hook 'iedit-mode-end-hook #'km-edit-iedit-cleanup)
        (lv-message msg)))))

(defun km-edit-iedit-cleanup ()
  "Remove the cleanup hook and delete the lv window if it exists."
  (require 'lv nil t)
  (remove-hook 'iedit-mode-end-hook #'km-edit-iedit-cleanup)
  (when (fboundp 'lv-delete-window)
    (lv-delete-window)))

(defun km-edit-default-ocurrence (&optional chars)
  "Return the substring of the buffer between characters matching CHARS.

Optional argument CHARS is a string of characters to consider as part of the
occurrence."
  (let* ((chars (or chars "_$A-Za-z0-9-"))
         (beg (save-excursion
                (skip-chars-backward chars)
                (point)))
         (end (save-excursion
                (skip-chars-forward chars)
                (point)))
         (name (buffer-substring-no-properties beg end)))
    (unless (string-empty-p name)
      name)))

;;;###autoload
(defun km-edit-setup-iedit-default-occurence ()
  "Set `iedit-default-occurrence-local' based on `major-mode'.

Example usage:

\\=(add-hook \\='after-change-major-mode-hook #\\='km-edit-setup-iedit-default-occurence)"
  (when-let* ((chars (cdr (or (assq major-mode
                                   km-edit-iedit-default-occurence-chars-alist)
                             (seq-find
                              (pcase-lambda (`(,k . ,_v))
                                (derived-mode-p k))
                              km-edit-iedit-default-occurence-chars-alist)))))
    (setq iedit-default-occurrence-local (apply-partially #'km-edit-default-ocurrence
                                                          chars))))

;;;###autoload
(defun km-edit-increase-number-at-point-or-move-text-up ()
  "Increase number at point or move text line up if no number exists."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'number)))
    (if (not bounds)
        (progn
          (require 'move-text nil t)
          (when (fboundp 'move-text-up)
            (call-interactively #'move-text-up)))
      (let* ((start (car bounds))
             (end (cdr bounds))
             (numstr (buffer-substring-no-properties start end))
             (pt (point))
             (dot-pos (string-match "\\." numstr))
             (curr-col (current-column))
             (new-str))
        (if (not dot-pos)
            (let ((num (string-to-number numstr)))
              (setq new-str (number-to-string (1+ num))))
          (let* ((frac-length (- (length numstr) dot-pos 1))
                 (offset (- pt start dot-pos 1))
                 (step
                  (if (or (< offset 0)
                          (>= offset frac-length))
                      (/ 1.0 (expt 10 frac-length))
                    (/ 1.0 (expt 10 (- frac-length offset)))))
                 (num (string-to-number numstr))
                 (newnum (+ num step))
                 (fmt (format "%%.%df" frac-length)))
            (setq new-str (format fmt newnum))))
        (replace-region-contents start end (lambda () new-str))
        (when (>= (length new-str)
                  (length numstr))
          (move-to-column curr-col))))))

;;;###autoload
(defun km-edit-decrease-number-at-point-or-move-text-down ()
  "Decrease the number at point or move the text line down."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'number)))
    (if (not bounds)
        (progn
          (require 'move-text nil t)
          (when (fboundp 'move-text-down)
            (call-interactively #'move-text-down)))
      (let* ((start (car bounds))
             (end (cdr bounds))
             (numstr (buffer-substring-no-properties start end))
             (pt (point))
             (dot-pos (string-match "\\." numstr))
             (curr-col (current-column))
             (new-str))
        (if (not dot-pos)
            (let ((num (string-to-number numstr)))
              (setq new-str (number-to-string (1- num))))
          (let* ((frac-length (- (length numstr) dot-pos 1))
                 (offset (- pt start dot-pos 1))
                 (step
                  (if (or (< offset 0)
                          (>= offset frac-length))
                      (/ 1.0 (expt 10 frac-length))
                    (/ 1.0 (expt 10 (- frac-length offset)))))
                 (num (string-to-number numstr))
                 (newnum (- num step))
                 (fmt (format "%%.%df" frac-length)))
            (setq new-str (format fmt newnum))))
        (replace-region-contents start end (lambda () new-str))
        (when (>= (length new-str)
                  (length numstr))
          (move-to-column curr-col))))))




(provide 'km-edit)
;;; km-edit.el ends here