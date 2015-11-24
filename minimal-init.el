;;; Minimal setup to load latest `mesa-mode'

;; activate debugging
;; (setq debug-on-error t
;;       debug-on-signal nil
;;       debug-on-quit t)

;; (setq edebug-trace t)

;; load the mode
(add-to-list 'load-path (expand-file-name "/home/jschwab/Software/mesa-major-mode/"))
(require 'mesa-mode)

;; set default MESA version
(setq mesa-default-version "r7624")

;; set default file associations
(add-to-list 'auto-mode-alist '("/inlist[^/]*$" . mesa-mode))
(add-to-list 'auto-mode-alist '("\\.defaults\\'" . (lambda () (mesa-mode) (view-mode))))
