;;; package --- init-packages
;;; Commentary:
;;; Install and setup packages here
;;; Code:
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defvar ddy/packages '(flyspell
		       key-chord
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
		       cl-lib
		       treemacs
		       popwin
		       diff-hl
		       ;; ivy
		       ivy
		       ivy-rich
		       counsel
		       swiper
		       counsel-projectile
		       ;; company
		       company
		       company-box
		       ;; rust
		       rust-mode
		       cargo
		       ;; haskell
		       haskell-mode
		       hindent
		       ;; web-dev
		       web-mode
		       ;; org
		       org
		       ox-twbs
		       ox-hugo
		       ;; themes
		       doom-themes
		       doom-modeline
		       ;; projectile
		       projectile
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

;; Find Executable Path on macOS
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;; defaults are here
(setq-default fill-column 80)

;; setup evil
(use-package evil
  :config (evil-mode 1))

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; setup company
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq help-at-pt-display-when-idle t)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; setup eglot
(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
	 (haskell-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (js-mode . eglot-ensure)
	 (java-mode . eglot-ensure)
	 (python-mode . eglot-ensure))
  :config (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

;; setup cargo
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; ox-hugo
(use-package ox-hugo
  :after ox)

;; ivy
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind (("\C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("C-h o" . counsel-describe-symbol)
	 ("C-x b" . ivy-switch-buffer)))
(use-package ivy-rich
  :config (ivy-rich-mode 1)
          (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; setup haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; setup hungry-delete
(use-package hungry-delete
   :config (global-hungry-delete-mode))

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)))

;; open init-packages.el
(defun open-init-package-file()
  "Open init-packages file."
  (interactive)
  (find-file "~/.emacs.d/lisp/init-packages.el"))
(global-set-key [f2] 'open-init-package-file)

;; setup org
(use-package org
  :init
  (set-language-environment "UTF-8")
  (setq org-src-fontify-natively t)
  (setq org-list-allow-alphabetical t)
  (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
  :hook ((org-mode . toggle-truncate-lines)
	 (org-mode . auto-fill-mode)))

;; setup projectile
(use-package projectile
  :init (setq projectile-project-search-path '("~/Developer"))
  :hook (prog-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode 1))

;; popwin
(use-package popwin
  :config (popwin-mode 1))

;; flyspell
(use-package flyspell
  :hook (org-mode . flyspell-mode))

(provide 'init-packages)
;;; init-packages.el ends here
