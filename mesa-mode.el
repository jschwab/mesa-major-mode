;;; -*- lexical-binding: t -*-
;;; mesa-mode.el --- a major mode for editing MESA inlist files

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

(defgroup mesa nil
  "A major mode for editing MESA inlist files"
  :prefix "mesa-")

(defcustom mesa-mode-hook nil
  "Hook to run upon entering `mesa-mode'"
  :type  'hook
  :group 'mesa)

(defcustom mesa-mode-enforce-formatting-default nil
  "Option to use strict formatting controls"
  :type 'boolean
  :group 'mesa)

(defun mesa-toggle-strict-formatting ()
  (interactive)
  "Switch between loose/strict formatting"
  ;; toggle boolean
  (setq-local mesa-mode-enforce-formatting (not mesa-mode-enforce-formatting))

  (if mesa-mode-enforce-formatting
      (setq-local indent-line-function 'mesa-mode-indent-line)
    (setq-local indent-line-function 'indent-relative))

  (if mesa-mode-enforce-formatting
      (message "Strict formatting is on")
    (message "Strict formatting is off")))

(defcustom mesa-indent-string
  "  "
  "String to use to indent lines; default is two spaces"
  :type 'string
  :group 'mesa)

(defcustom mesa-tags-file-path
  "TAGS"
  "Name of the TAGS file inside of your MESA project"
  :type 'string
  :group 'mesa)

(defcustom mesa-tags-regexp
  "'/^[ \\t]+\\([^ \\t!()]+\\)\\(([:,1]+)\\)?[ \\t]*=/\\1/'"
  "Regexp to recognize tags in defaults files"
  :type 'regexp
  :group 'mesa
)

(defcustom mesa-use-remote-paths
  nil
  "If t, use remote paths for tramp buffers; if nil, always use local paths."
  :type 'boolean
  :group 'mesa
)

(defconst mesa-defaults-files
  '("star/defaults/star_job.defaults"
    "star/defaults/controls.defaults"
    "star/defaults/pgstar.defaults"
    "binary/defaults/binary_job.defaults"
    "binary/defaults/binary_controls.defaults")
  "Defaults files contained in MESA")

(defun mesa~prepend-system-name (filename)
  "Given a filename, (possibly) prepend the remote system name"
  (let ((remote (file-remote-p (buffer-file-name))))
    (if (and mesa-use-remote-paths remote)
        (concat remote filename)
      filename)))

(defun mesa~prepend-mesa-dir (filename)
  "Prepend the MESA_DIR to a filename"
  (let ((mesa-dir (mesa-version-get-mesa-dir)))
    (mesa~prepend-system-name
       (concat (file-name-as-directory mesa-dir) filename))))

(defun mesa-tags-file ()
  "Create the full path to the TAGS directory"
  (mesa~prepend-mesa-dir mesa-tags-file-path))

(defun mesa-visit-tags-table ()
  "Visit/change tags table.  We can't use a local table because of long-standing bugs"

  ;; make TAGS file if it doesn't exist
  (if (not (file-exists-p (mesa-tags-file)))
      (mesa-regen-tags))

  (let ((tags-add-tables nil))
    (visit-tags-table (mesa-tags-file))))

(defun mesa-regen-tags ()
  "Regenerate the tags file for the MESA defaults directory"
  (interactive)
  (let ((default-directory (mesa~prepend-mesa-dir nil)))
    (if (file-exists-p default-directory)
        (shell-command (format "etags --language=none --regex=%s -o %s %s"
                               mesa-tags-regexp
                               mesa-tags-file-path
                               (mapconcat 'identity mesa-defaults-files " ")))
      (message "Failed to generate TAGS: %s does not exist" default-directory))))

(defun mesa-toggle-boolean ()
  "Toggle an inlist flag between .true. <--> .false."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward ".true.\\|.false." (line-end-position) t)
        (replace-match (if (string-equal (match-string 0) ".true.")
                           ".false." ".true.") nil nil))))

;; borrowed from http://www.emacswiki.org/emacs/CommentingCode
(defun mesa-comment-dwim (&optional arg)
  "Replaces default behavior of comment-dwim, when it inserts
comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; adapted from http://www.emacswiki.org/emacs/EmacsTags#tags
(defun mesa-view-tag-default-other-window ()
  "Like as `find-tag-other-window' but doesn't move the point"
  (interactive)
  (let ((window (get-buffer-window))
        (identifier (mesa-find-tag-default)))
    (if (not identifier)
        (message "No option on this line")
      (xref-find-definitions-other-window identifier)
      (recenter nil)
      (select-window window))))

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
          (insert mesa-indent-string)))

    (goto-char (marker-position old-point))))

(defun mesa-mode-before-save-hook ()
  (when mesa-mode-enforce-formatting
    (indent-region (point-min) (point-max))
    (whitespace-cleanup)))

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


;; The function mesa-inside-namelist is based on f90-namelist-mode.el
;; License: GPL v2+
;; Copyright: D. Dickinson and P. Hill
;; URL: https://github.com/ZedThree/f90-namelist-mode

(defun mesa-inside-namelist ()
  "Returns the name of the namelist if inside a namelist and nil if not"
  (let (start end namelist)
    (save-excursion
      (setq start (re-search-backward mesa-namelist-start-re 0 t))
      (if start
          (setq namelist (match-string-no-properties 1))
        (setq start 0)))
    (save-excursion
      (setq end (re-search-backward mesa-namelist-end-re 0 t))
      (when (not end) (setq end 0)))
    (if ( > start end) namelist)))


(defun mesa-edit-value (&optional arg)
  "Edit the value portion of a line"
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward mesa-namelist-key-value-re (line-end-position) t)
        (replace-match (read-string "Value: " (if arg (match-string-no-properties 3))) nil nil nil 3)
      (message "Line is not a key-value pair"))))


(defun mesa-edit-index (&optional start end index)
  "Replace array indicies between START and END with INDEX.  If called
interactively, START and END are the start/end of the region if the
mark is active, or of the line f the mark is inactive."
  (interactive (let ((index (read-string "Index: ")))
                 (if (use-region-p)
                     (list (region-beginning) (region-end) index)
                   (list (line-beginning-position) (line-end-position) index))))
  (save-excursion
    (goto-char start)
    (while (re-search-forward mesa-namelist-key-value-re end t)
      (unless (string= "" (match-string 2))
          (replace-match index nil nil nil 2)))))


(defun mesa-reset-to-default ()
  "Reset a key-value pair to the default"
  (interactive)
  (let (tagname tagbuffer tagline tagdefault)

    ;; extract default tag line
    (save-current-buffer
      (setq tagname (mesa-find-tag-default))
      (setq tagbuffer (find-tag-noselect tagname)))
    (with-current-buffer tagbuffer
      (setq tagline (thing-at-point 'line t)))

    ;; extract default tag value
    (string-match mesa-namelist-key-value-re tagline)
    (setq tagdefault (match-string 3 tagline))

    ;; replace tag name
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward mesa-namelist-key-value-re (line-end-position) t)
          (if (string= tagname (match-string 1))
              (progn
                (message "Resetting tag to %s" tagdefault)
                (replace-match tagdefault nil nil nil 3))
            (message "Bad tag"))
        (message "Line is not a key-value pair")))))


(defun mesa~pgstar-list-plots ()
  "List the known PGSTAR plots"
  (let ((filename (mesa~prepend-mesa-dir "star/defaults/pgstar.defaults"))
        (matches))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents filename)
        (while (re-search-forward "\\([A-Za-z0-9_]+\\)_win_flag" nil t 1)
          (push (match-string 1) matches))))
    matches))


(defun mesa~pgstar-insert-plot-lines-only (plot)
  "Insert the PGSTAR options that start with PLOT"
  (let ((filename (mesa~prepend-mesa-dir "star/defaults/pgstar.defaults"))
        (plot-regexp (format "^[[:blank:]]*%s_" plot))
        (plot-options))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents filename)
        (while (re-search-forward plot-regexp nil t 1)
          (push (thing-at-point 'line t) plot-options))))
    (insert (mapconcat 'identity (reverse plot-options) ""))))


(defun mesa~pgstar-insert-plot-entire-section (plot)
  "Insert the PGSTAR options from the plot section"
  (let ((filename (mesa~prepend-mesa-dir "star/defaults/pgstar.defaults"))
        (plot-regexp (format "^[[:blank:]]*%s_win_flag" plot))
        (beg)
        (end)
        (match))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents filename)
        (re-search-forward plot-regexp nil t 1)
        (beginning-of-line)
        (setq beg (point))
        (search-forward "!----" nil t 1)
        (forward-line -1)
        (setq end (point))
        (setq match (buffer-substring beg end))))
    (insert match)))


(defun mesa-insert-pgstar-templates ()
  "Insert pgstar template"
  (interactive)
  (when (string= (mesa-inside-namelist) "pgstar")
    (setq pgstar-plot
          (completing-read "Select plot: "(mesa~pgstar-list-plots) nil t))
    (mesa~pgstar-insert-plot-entire-section pgstar-plot)))


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
(defvar mesa-namelist-key-value-re
  (concat
   "^[[:blank:]]*"                       ; leading whitespace
   "\\([a-zA-Z0-9_]*\\)"                 ; key
   "(?\\([0-9,]?+\\))?"                  ; (indicies)?
   "[[:blank:]]*=[[:blank:]]*"           ; equals
   "[\"\']?\\([-+a-zA-Z0-9_.]*\\)[\"\']?"; value
   "[[:blank:]]*"                        ; trailing whitespace
   )
  "Regexp for matching namelist key-value pair")

(defconst mesa-namelist-boolean-keywords
  '(".true." ".false.")
  "Namelist boolean keywords.")

(defvar mesa-namelist-value-boolean-re
  (regexp-opt mesa-namelist-boolean-keywords 'paren)
  "Regexp for matching namelist value (boolean)")

(defvar mesa-namelist-value-number-re
  "=[[:blank:]]*\\([-+0-9\\.eEdD]+\\)[[:blank:]]*"
  "Regexp for matching namelist value (number)")

(defconst mesa-font-lock-keywords
  (list

   ;; highlight the start of each namelist
   (cons mesa-namelist-start-re '(1 font-lock-function-name-face))

   ;; highlight keys
   (cons mesa-namelist-key-value-re '(1 font-lock-variable-name-face))

   ;; highlight values
   ;; values could be strings, booleans, or numbers
   ;; strings are taken care of by the syntax tables
   ;; put booleans in builtin-face
   (cons mesa-namelist-value-boolean-re '(1 font-lock-builtin-face))
   ;; put numbers in constant-face
   (cons mesa-namelist-value-number-re '(1 font-lock-constant-face))

   )
  "Font lock keywords for namelist files")


;;; Bindings

(defvar mesa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'mesa-comment-dwim)
    (define-key map "\C-c\C-d" 'mesa-view-tag-default-other-window)
    (define-key map "\C-c\C-e" 'mesa-edit-value)
    (define-key map "\C-c\C-f" 'mesa-toggle-strict-formatting)
    (define-key map "\C-c\C-i" 'mesa-edit-index)
    (define-key map "\C-c\C-r" 'mesa-reset-to-default)
    (define-key map "\C-c\C-p" 'mesa-insert-pgstar-templates)
    (define-key map "\C-c\C-t" 'mesa-toggle-boolean)
    (define-key map "\M-." 'mesa-find-defintions)
    map)
  "Key map for `mesa-mode'.")


(defun mesa-find-tag-default ()

  ;; switch tags-table if needed
  (unless (string= tags-file-name (mesa-tags-file))
    (message "Swiching TAGS table")
    (mesa-visit-tags-table))

  (save-excursion
    (beginning-of-line)
    (when (re-search-forward mesa-namelist-key-value-re (line-end-position) t)
      (match-string-no-properties 1))))

(defun mesa-find-defintions ()
  "Wrapper for xref--find-definitions."
  (interactive)
  (let ((identifier (mesa-find-tag-default)))
    (if identifier
        (xref--find-definitions identifier nil)
      (message "No option on this line"))))

;;;###autoload
(defun mesa-mode-xref-backend ()
  "Mesa-Mode backend for Xref."
  'xref-mesa)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-mesa)))
  (mesa-find-tag-default))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-mesa)))
  (tags-lazy-completion-table))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-mesa)) symbol)
  (etags--xref-find-definitions symbol))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-mesa)) symbol)
  (etags--xref-find-definitions symbol t))


;;;###autoload
(define-derived-mode mesa-mode prog-mode "mesa"
  "A major mode for editing MESA inlist files"
  :syntax-table mesa-mode-syntax-table
  :find-tag-default-function mesa-find-tag-default
  :group 'mesa

  ;; specify mode name
  (setq-local mode-name "MESA")

  ;; specify comment characters
  (setq-local comment-start "! ")
  (setq-local comment-start-skip "!+\\s-*")

  ;; font-lock
  (setq-local font-lock-defaults '(mesa-font-lock-keywords))

  ;; formatting controls
  (setq-local indent-tabs-mode nil)

  ;; set local formatting choice to default
  (setq-local mesa-mode-enforce-formatting mesa-mode-enforce-formatting-default)

  ;; make sure we begin with no mesa-version set
  (setq-local mesa-version-buffer nil)

  (progn

    ;; this stuff doesn't need to happen for .defaults files
    (unless (string-match "\\.defaults$" (buffer-file-name))

      ;; activate mesa-version minor mode
      (mesa-version-minor-mode 1)

      ;; visit the appropriate tags table
      (mesa-visit-tags-table)

      ;; activate the pre-save hook
      (add-hook 'before-save-hook 'mesa-mode-before-save-hook nil t)))

  ;; hooks
  (add-hook 'xref-backend-functions #'mesa-mode-xref-backend nil t)
  (run-hooks 'mesa-mode-hook))

(provide 'mesa-mode)

;;; mesa-mode.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
