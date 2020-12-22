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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "aaf300544667970333366f2bad847899f193fcfe96172ec325dbc3195b797220" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(package-selected-packages
   '(smart-mode-line-atom-one-dark-theme use-package company hungry-delete swiper counsel ivy smartparens expand-region iedit flycheck flycheck-rust exec-path-from-shell nyan-mode yasnippet auto-yasnippet company-lsp company-box lsp-mode lsp-treemacs lsp-ui lsp-python-ms lsp-haskell js2-mode nodejs-repl web-mode htmlize treemacs treemacs-icons-dired treemacs-magit all-the-icons haskell-mode flyspell-popup rust-mode cargo magit tuareg org-ref ox-twbs doom-themes evil ivy-bibtex esup smart-mode-line smart-mode-line-powerline-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
