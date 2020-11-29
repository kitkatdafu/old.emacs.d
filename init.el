;;; package --- init
;;; Commentary:
;;; Hello World, this is the beginning
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp")

;; circumvent signature check
(setq package-check-signature nil)

(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-keybindings)
;;; init.el ends here
