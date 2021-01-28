;;; package --- init-packages

;;; Commentary:
;;; Install and setup packages here
;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defvar ddy/packages '(
	       use-package
               hungry-delete
               smartparens
	       evil
	       all-the-icons
	       nyan-mode
	       exec-path-from-shell
	       swiper
	       yasnippet
	       eglot
	       markdown-mode
	       magit
	       treemacs
	       ;; company
               company
	       company-box
	       ;; helm
	       helm
	       helm-ls-git
	       swiper-helm
	       ;; rust
	       rust-mode
	       cargo
	       ;; org
	       org
	       ox-twbs
	       ox-hugo
	       ;; themes
	       doom-themes
	       doom-modeline
	       ) "Default packages.")

(setq package-selected-packages ddy/packages)

(defun ddy/packages-installed-p ()
  "Install packages."
  (cl-loop for pkg in ddy/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (ddy/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ddy/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; setup evil
(require 'evil)
(evil-mode 1)

;; setup company
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq help-at-pt-display-when-idle t)
  )
(use-package company-box
  :hook (company-mode . company-box-mode))

;; setup helm
(helm-mode 1)
(require 'helm-ls-git)

;; setup eglot
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

;; setup cargo
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; ox-hugo
(use-package ox-hugo
  :after ox)

(provide 'init-packages)
;;; init-packages.el ends here
