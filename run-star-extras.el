;;; -*- lexical-binding: t -*-
;;; run-star-extras.el --- a minor mode for editing MESA run_star_extras files

;; Author: Josiah Schwab <jschwab@gmail.com>
;; Keywords: files

;; Copyright (C) 2013-2016 Josiah Schwab

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

(require 'mesa-version)

(defgroup rse nil
  "A minor mode for editing MESA run_star_extras.f files"
  :prefix "rse-")


(defcustom rse-mode-line
  " RSE"
  "Mode line lighter for run-star-extras minor mode"
  :type 'sexp
  :risky t
  :group 'rse)


(defconst rse-include-line
  "      include 'standard_run_star_extras.inc'"
  "Line in run_star_extras.f that pulls in default code.")


(defcustom rse-before-save-hook nil
  "Hook to run before saving inlist"
  :type  'hook
  :group 'rse)

(defcustom rse-after-save-hook nil
  "Hook to run after saving inlist"
  :type  'hook
  :group 'rse)


(defcustom rse-update-extra-column-counts nil
  "Option to automatically update extra column counts on save"
  :type 'boolean
  :group 'rse)

(defcustom rse-recompile-on-save nil
  "Option to automatically recompile on save"
  :type 'boolean
  :group 'rse)


(defun rse~prepend-mesa-dir (filename)
  "Prepend the MESA_DIR to a filename"
  (let ((mesa-dir (mesa-version-get-mesa-dir)))
    (concat (file-name-as-directory mesa-dir) filename)))


(defun rse-enable ()
  "Enable run_star_extras.f by inserting the standard include file"
  (interactive)
  (let ((filename (rse~prepend-mesa-dir "include/standard_run_star_extras.inc")))
    (if (file-exists-p filename)
        (save-excursion
          (beginning-of-buffer)
          (if (search-forward rse-include-line nil nil)
              (save-restriction
                (narrow-to-region (line-beginning-position) (line-end-position))
                (insert-file-contents filename nil nil nil t))))
      (message "Failed to insert file: %s does not exist" filename))))


(defmacro narrow-to-subroutine (subroutine &rest body)
  "Execute BODY in a region narrowed to SUBROUTINE"
  `(let ((beg-pattern (format "subroutine %s" subroutine))
         (end-pattern (format "end subroutine %s" subroutine)))
     (save-excursion
       (goto-char (point-min))
       (let* ((point-beg (search-forward beg-pattern nil t))
              (point-end (search-forward end-pattern nil t)))
         (when (and (and point-beg point-end) (> point-end point-beg))
           (save-restriction
             (narrow-to-region point-beg point-end)
             (goto-char (point-min))
             ,@body))))))


(defun rse~find-max-index-in-subroutine (subroutine thing-with-index)
  "Find the maximum index used in a subroutine named SUBROUTINE.
  THING-WITH-INDEX is a regexp whose first group is the index
  that you want to find the max."
  (let ((max-index 0))
    (narrow-to-subroutine
     subroutine
     (while (re-search-forward thing-with-index (point-max) t)
       (setq max-index (max max-index (string-to-number (match-string-no-properties 1))))))
    max-index))


(defconst rse-thing-to-count
  "names(\\([[:digit:]]+\\))"
  "Thing to look at to count extra columns")

(defun rse~count-extra-columns (arg)
  "Count in subroutine named data_for_extra_ARG_columns"
  (let ((subroutine-name (format "data_for_extra_%s_columns" arg)))
    (rse~find-max-index-in-subroutine subroutine-name rse-thing-to-count)))

(defun rse~update-integer-variable (variable new-val)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (format "%s = \\([[:digit:]]+\\)" variable) (point-max) t)
      (unless (string= "" (match-string 1))
        (replace-match (number-to-string new-val) nil nil nil 1)))))

(defun rse~update-how-many-extra-columns (arg)
  "Update variable how_many_extra_ARG_columns"
  (let ((variable-name (format "how_many_extra_%s_columns" arg))
        (how-many (rse~count-extra-columns arg)))
    (rse~update-integer-variable variable-name how-many)))

(defun rse-before-save-hook ()
  (when rse-update-extra-column-counts
    (let ((rse-file-type (file-name-nondirectory (buffer-file-name))))
      (when (string= "run_star_extras.f" rse-file-type)
        (rse~update-how-many-extra-columns "history")
        (rse~update-how-many-extra-columns "profile"))
      (when (string= "run_binary_extras.f" rse-file-type)
        (rse~update-how-many-extra-columns "binary_history")))))

(defun rse-compile-with-enviroment ()
  "Compile in an enviroment where MESA_DIR is from mesa-version"
  (interactive)
  (let* ((mesa-dir (mesa-version-get-mesa-dir))
         (compilation-environment `(,(format "MESA_DIR=%s" mesa-dir))))
    (compile compile-command)))

(defun rse-after-save-hook ()
  (when rse-recompile-on-save
    (rse-compile-with-enviroment)))

(define-minor-mode run-star-extras-minor-mode
  "Toggle run-star-extras minor mode in the usual way."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter rse-mode-line
  ;; The minor mode bindings.
  :keymap
  '(
    ("\C-c\C-c" . rse-compile-with-enviroment)
    ("\C-c\C-r" . rse-enable)
    )
  ;; The body
  (if run-star-extras-minor-mode

      ;; turn run-star-extras-minor-mode on
      (progn
        ;; turn on mesa-version minor mode
        (mesa-version-minor-mode 1)

        ;; add hooks
        (add-hook 'before-save-hook 'rse-before-save-hook nil t)
        (add-hook 'after-save-hook 'rse-after-save-hook nil t)

        ;; set the compile command
        (setq-local compilation-read-command nil)
        (setq-local compile-command "cd ../ && ./clean && ./mk"))

  ;; turn run-star-extras-minor-mode off
    (progn
      (remove-hook 'before-save-hook 'rse-before-save-hook t)
      (remove-hook 'after-save-hook 'rse-after-save-hook t)))
  
  ;; the group
  :group 'rse)

(provide 'run-star-extras)

;;; run-star-extras.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
