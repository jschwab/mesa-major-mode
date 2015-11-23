;;; -*- lexical-binding: t -*-
;;; mesa-mode.el --- a major mode for editing MESA inlist files

;; Author: Josiah Schwab <jschwab@gmail.com>
;; Keywords: files

;; Copyright (C) 2013-2015 Josiah Schwab

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:

;; See README.org

;;; Code:

(defgroup mesa nil
  "A major mode for editing MESA inlist files"
  :prefix "mesa-")

(defcustom mesa-mode-hook nil
  "Hook to run upon entering `mesa-mode'"
  :type  'hook
  :group 'mesa)

(defcustom mesa-mode-before-save-hook nil
  "Hook to run before saving inlist"
  :type  'hook
  :group 'mesa)

(defun mesa-mode-indent-line ()
  "indent the current line"
  (let ((old-point (point-marker)))
    (beginning-of-line)
    (delete-horizontal-space)

    ;; don't indent namelist parts
    (if (mesa-inside-namelist)
        (unless
            (or
             (re-search-forward mesa-namelist-start-re (line-end-position) t)
             (re-search-forward mesa-namelist-end-re (line-end-position) t))
          (insert "  ")))

    (goto-char (marker-position old-point))))

(defun mesa-mode-before-save-hook ()
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(defcustom mesa-comment-start-re "[!#]+"
  "Regexp for start of a comment"
  :type  'regexp
  :safe  'stringp
  :group 'mesa)

(defcustom mesa-comment-line-re (format "^*%s" mesa-comment-start-re)
  "Regexp for a comment line"
  :type  'regexp
  :safe  'stringp
  :group 'mesa)

(defcustom mesa-namelist-start-re "^[[:blank:]]*&\\([a-zA-Z0-9_]+\\)"
  "Namelist start regexp, note we only match namelists with a name"
  :type  'regexp
  :safe  'stringp
  :group 'mesa)

(defcustom mesa-namelist-end-re "^[[:blank:]]*/"
  "Namelist end regexp"
  :type  'regexp
  :safe  'stringp
  :group 'mesa)

(defun mesa-inside-namelist ()
  "Returns t if currently inside a namelist and nil if not"
  (setq st (save-excursion (re-search-backward mesa-namelist-start-re 0 t)))
  (setq en (save-excursion (re-search-backward mesa-namelist-end-re 0 t)))
  (when (not st)
    (setq st 0))
  (when (not en)
    (setq en 0))
  ( > st en))

;;; Syntax table
(defvar mesa-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\! "<"  st) ; ! begins comment
    (modify-syntax-entry ?\n ">"  st) ; newline ends comment
    (modify-syntax-entry ?_  "_"  st) ; underscores are in variable names
    (modify-syntax-entry ?\' "\"" st) ; single quotes are quotes
    (modify-syntax-entry ?\" "\"" st) ; double quotes are quotes
    st)
  "Syntax table used in mesa-mode.")


;;; Font lock.
(defvar mesa-namelist-key-re
  (concat
   "^[[:blank:]]*"                      ; leading whitespace
   "\\([a-zA-Z0-9_]*\\)"                ; key
   "(?[0-9,]?+)?"                       ; (indicies)?
   "[[:blank:]]*="                      ; equals
   )
  "Regexp for matching namelist key")

(defconst mesa-namelist-boolean-keywords
  '(".true." ".false.")
  "Namelist boolean keywords.")

(defvar mesa-namelist-value-boolean-re
  (regexp-opt mesa-namelist-boolean-keywords 'paren)
  "Regexp for matching namelist value (boolean)")

(defvar mesa-namelist-value-number-re
  "=[[:blank:]]*\\([0-9\\.eEdD-]+\\)[[:blank:]]*"
  "Regexp for matching namelist value (number)")

(defconst mesa-font-lock-keywords
  (list

   ;; highlight the start of each namelist
   (cons mesa-namelist-start-re '(1 font-lock-function-name-face))

   ;; highlight keys
   (cons mesa-namelist-key-re '(1 font-lock-variable-name-face))

   ;; highlight values
   ;; values could be strings, booleans, or numbers
   ;; strings are taken care of by the syntax tables
   ;; put booleans in builtin-face
   (cons mesa-namelist-value-boolean-re '(1 font-lock-builtin-face))
   ;; put numbers in constant-face
   (cons mesa-namelist-value-number-re '(1 font-lock-constant-face))

   )
  "Font lock keywords for namelist files")

;;;###autoload
(define-derived-mode mesa-mode prog-mode "mesa"
  "A major mode for editing MESA inlist files"
  :syntax-table mesa-mode-syntax-table
  :group 'mesa

  ;; font-lock
  (setq-local font-lock-defaults '(mesa-font-lock-keywords))

  ;; require strict formatting
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'indent-line-function) 'mesa-mode-indent-line)

  ;; hooks
  (add-hook 'before-save-hook 'mesa-mode-before-save-hook nil t)
  (run-hooks 'mesa-mode-hook))

(provide 'mesa-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; mesa-mode.el ends here
