;;; Minimal setup to load latest `mesa-mode'

;; load the mode
;;   change /path/to/mesa-major-mode to be the path to the
;;   the folder where you placed the mesa-mode.el file
(add-to-list 'load-path "/path/to/mesa-major-mode/")
(require 'mesa-mode)
(require 'run-star-extras)

;; set default MESA version
(setq mesa-default-version "r8845")

;; set default file associations
(add-to-list 'auto-mode-alist '("/inlist[^/]*$" . mesa-mode))
(add-to-list 'auto-mode-alist '("\\.defaults$" . (lambda () (mesa-mode) (view-mode))))
(add-to-list 'auto-mode-alist '("/run_star_extras.f$" . (lambda () (f90-mode) (run-star-extras-minor-mode))))
