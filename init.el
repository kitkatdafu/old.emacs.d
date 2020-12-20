;;; package --- init
;;; Commentary:
;;; Hello World, this is the beginning
;;; Code:
(setq gc-cons-threshold (* 50 1000 1000)) ;;
;; (setq read-process-output-max 3145728) ;;

(add-to-list 'load-path "~/.emacs.d/lisp")

;; circumvent signature check
(setq package-check-signature nil)

(require 'init-packages)
(use-package init-ui)
(use-package init-org)
(use-package init-better-defaults)
(use-package init-keybindings)

(setq gc-cons-threshold (* 2 1000 1000))
;;; init.el ends here
