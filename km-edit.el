;;; km-edit.el --- Miscellaneous editing utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-edit
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "26.1"))
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
  (when-let ((overlay (make-overlay beg end))
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
  (when-let ((bounds (or (and (region-active-p)
                              (car (region-bounds)))
                         (bounds-of-thing-at-point 'sexp))))
    (if (called-interactively-p 'any)
        (kill-new (buffer-substring-no-properties
                   (car bounds)
                   (cdr bounds)))
      (buffer-substring-no-properties
       (car bounds)
       (cdr bounds)))))

(defun km-edit-get-region ()
  "Return current active region as string or nil."
  (when (and (region-active-p)
             (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning)
                  (region-end)))))

;;;###autoload
(defun km-edit-copy-prin1-to-string-at-point ()
  "Return a string with the printed representation of active region or sexp."
  (interactive)
  (when-let ((content (km-edit-copy-sexp-or-region-at-point)))
    (if (called-interactively-p 'any)
        (kill-new (prin1-to-string content))
      (prin1-to-string content))))

;;;###autoload
(defun km-edit-copy-prin1-to-string-no-newlines ()
  "Return a string with the printed representation of region without new lines."
  (interactive)
  (require 'subr-x)
  (when-let ((content (string-join
                       (split-string
                        (km-edit-copy-sexp-or-region-at-point)
                        (if (yes-or-no-p "Remove multi spaces?")
                            nil "\n")
                        t)
                       "\s")))
    (if (called-interactively-p 'any)
        (kill-new (prin1-to-string content))
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
      (kill-new rep)
      (message "Copied:\n%s" rep)
      rep)))


(defun km-edit-elisp-to-doc-str (str)
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
(defun km-edit-copy-as-elisp-doc-str (beg end)
  "Copy region as Elisp doc string.

Argument BEG is the position of the beginning of the region.

Argument END is the position of the end of the region.

Argument STR is a string to be escaped for use in documentation strings."
  (interactive "r")
  (when (and (region-active-p)
             (use-region-p))
    (let ((rep
           (km-edit-elisp-to-doc-str
            (buffer-substring-no-properties beg end))))
      (kill-new rep)
      (message "Copied:\n%s" rep)
      rep)))

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
          (regex)
          (rep))
      (when start (save-excursion
                    (goto-char start)
                    (forward-sexp 1)
                    (setq end (point))))
      (setq regex (buffer-substring-no-properties start end))
      (when regex
        (setq rep (with-temp-buffer
                    (let ((indent-tabs-mode nil))
                      (when (fboundp 'xr--rx-to-string)
                        (xr--rx-to-string regex)))
                    (concat "(rx " (replace-regexp-in-string "[\t]" "\s"
                                                             (buffer-string))
                            ")")))
        (km-edit-confirm-and-replace-region start end rep)))))


;;;###autoload
(defun km-edit-split-string ()
  "Split active region to strings.
For example, inside string:

\"add html head extra\" => \"add\" \"html\" \"head\" \"extra\"

a b => \"a\" \"b\"."
  (interactive)
  (pcase-let
      ((`(,start . ,end)
        (if (and
             (region-active-p)
             (use-region-p))
            (cons (region-beginning)
                  (region-end))
          (when-let ((str-start (nth 8 (syntax-ppss (point)))))
            (goto-char str-start))
          (when (looking-at "\"")
            (cons (point)
                  (save-excursion
                    (forward-sexp 1)
                    (point)))))))
    (when-let* ((reg
                 (when (and start end)
                   (buffer-substring-no-properties start end)))
                (replacement (mapconcat #'prin1-to-string
                                        (split-string
                                         (if (string-match-p "^\""
                                                             reg)
                                             (condition-case  nil
                                                 (car-safe (read-from-string
                                                            reg))
                                               (error reg))
                                           reg))
                                        "\s"))
                (overlay (make-overlay start end)))
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
          (replace-region-contents start end (lambda () replacement)))))))

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
  :lighter " kme-cln"
  :global nil
  (if km-edit-whitespace-cleanup-mode
      (add-hook 'before-save-hook #'km-edit-remove-spaces-between-empty-lines
                nil 'local)
    (remove-hook 'before-save-hook #'km-edit-remove-spaces-between-empty-lines
                 'local)))



(provide 'km-edit)
;;; km-edit.el ends here