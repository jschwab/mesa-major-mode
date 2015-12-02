;;; Minimal setup to load latest `mesa-mode'

;; load the mode
;;   change /path/to/mesa-major-mode to be the path to the
;;   the folder where you placed the mesa-mode.el file
(add-to-list 'load-path "/path/to/mesa-major-mode/")
(require 'mesa-mode)

;; set default MESA version
(setq mesa-default-version "r7624")

;; set default file associations
(add-to-list 'auto-mode-alist '("/inlist[^/]*$" . mesa-mode))
(add-to-list 'auto-mode-alist '("\\.defaults\\'" . (lambda () (mesa-mode) (view-mode))))
