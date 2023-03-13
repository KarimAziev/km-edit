;;; km-edit.el --- Miscellaneous editing utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-edit
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "26.1"))

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
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

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


;;;###autoload
(defun km-edit-dwim-insert-key-description ()
  "Read and insert a description of keystrokes.
Inside string also remove old content.
If parent form is `define-key' and point is not inside string,
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
                  (eq 'define-key (car-safe
                                   (sexp-at-point))))))))
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

(provide 'km-edit)
;;; km-edit.el ends here