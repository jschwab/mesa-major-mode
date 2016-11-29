;;; -*- lexical-binding: t -*-
;;; mesa-version.el --- a minor mode for switching MESA versions

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

(defgroup mesa-version nil
  "A minor mode for controling the MESA version"
  :prefix "mesa-version-")

(defcustom mesa-version-mesa-dir
  nil
  "If this is set, it takes precedence over the enviroment
variable MESA_DIR."
  :type 'file
  :group 'mesa
)

(defcustom mesa-version-default
  nil
  "Default version of MESA"
  :type 'string
  :group 'mesa-version
  )

(defcustom mesa-version-init-file
  "~/.mesa_init"
  "Default MESA init file"
  :type 'file
  :group 'mesa-version
)

;; The function mesa-read-init is based on ini.el
;; License: GPL v2+
;; Copyright: Daniel Ness
;; URL: https://github.com/daniel-ness/ini.el

(defun mesa-version-read-init (filename)
  "Read a MESA config file"
  (let ((lines (with-temp-buffer
                 (insert-file-contents filename)
                 (split-string (buffer-string) "\n")))
        (section)
        (section-list)
        (alist))
    (dolist (l lines)
      (cond ((string-match "^;" l) nil)
            ((string-match "^[ \t]$" l) nil)
            ((string-match "^\\[\\(.*\\)\\]$" l)
             (progn
               (if section
                   ;; add as sub-list
                   (setq alist (cons `(,section . ,section-list) alist))
                 (setq alist section-list))
               (setq section (match-string 1 l))
               (setq section-list nil)))
            ((string-match "^[ \t]*\\(.+\\) = \\(.+\\)$" l)
             (let ((property (match-string 1 l))
                   (value (match-string 2 l)))
               (progn
                 (setq section-list (cons `(,property . ,value) section-list)))))))
    (if section
        ;; add as sub-list
        (setq alist (cons `(,section . ,section-list) alist))
      (setq alist section-list))
    alist))

(defun mesa-version-list-versions ()
  "List the possible MESA versions"
  (let ((mesa-init-data (mesa-version-read-init mesa-version-init-file)))
    (mapcar 'car mesa-init-data)))

(defun mesa-dir-from-version (version)
  "Given a MESA version string, return the corresponding MESA_DIR"
  (let ((mesa-init-data (mesa-version-read-init mesa-version-init-file)))
    (cdr (assoc "MESA_DIR"
                (cdr (assoc version mesa-init-data))))))

(defun mesa-version-get-mesa-dir ()
  "Get a value for MESA_DIR"
  (cond
   (mesa-version-buffer (mesa-dir-from-version mesa-version-buffer))
   (mesa-version-mesa-dir mesa-version-mesa-dir)
   (t (getenv "MESA_DIR"))))

(defun mesa-version-change ()
  "Change the MESA version being used in this buffer"
  (interactive)
  (setq-local mesa-version-buffer
                (completing-read
                 "Select MESA Version: "
                 (mesa-version-list-versions)
                 nil t)))

(defcustom mesa-version-mode-line
  '(:eval (format " [%s]" mesa-version-buffer))
  "Mode line lighter for mesa-version minor mode"
  :type 'sexp
  :risky t)

(define-minor-mode mesa-version-minor-mode
  "Toggle mesa-version minor mode in the usual way."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter mesa-version-mode-line
  ;; The minor mode bindings.
  :keymap
  '(
    ("\C-c\C-v" . mesa-version-change)
    )
  ;; The body
  (if mesa-version-minor-mode

      ;; turn mesa-version-minor-mode on
      (progn

        ;; set the MESA version
        (setq-local mesa-version-buffer mesa-version-default))

  ;; turn mesa-version-minor-mode off
    (progn
      (remove-hook 'before-save-hook 'rse-before-save-hook t)))
  
  ;; the group
  :group 'mesa-version)

(provide 'mesa-version)

;;; mesa-version.el ends here

;; Local Variables:
;; coding: utf-8
;; End:
