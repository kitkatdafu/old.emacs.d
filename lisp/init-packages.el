;;; package --- init-packages
;;; Commentary:
;;; Install and setup packages here
;;; Code:
(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize))

(setq package-archives '(
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("melpa"        . "https://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ))



;; Add Packages
(defvar ddy/packages '(
	       use-package
               company
               hungry-delete
	       swiper
	       counsel
	       ivy
               smartparens
	       ;; popwin
	       expand-region
	       iedit
	       flycheck
	       flycheck-rust
               exec-path-from-shell
	       nyan-mode
	       yasnippet
	       auto-yasnippet
	       company-lsp
	       company-box
	       lsp-mode
	       lsp-treemacs
	       lsp-ui
	       lsp-python-ms
	       lsp-haskell
	       js2-mode
	       nodejs-repl
	       web-mode
	       htmlize
	       treemacs
	       treemacs-icons-dired
	       treemacs-magit
	       all-the-icons
	       haskell-mode
	       flyspell-popup
	       rust-mode
	       cargo
	       magit
	       tuareg
	       org-ref
	       ox-twbs
	       doom-themes
	       evil
	       esup
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

;; enable flycheck
(add-hook 'prog-mode-hook 'flycheck-mode)

;; evil mode
(use-package evil
  :config
  (evil-mode 1))

;; setup swiper/ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; setup dwim
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))

;; setup lsp-mode, tuning, and lsp-ui
(use-package lsp-mode
  :defer t
  :config
  (setq lsp-enable-file-watchers nil)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-prefer-flymake nil) ;; prefer flycheck
  (setq lsp-enable-indentation t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 1)
  )

;; setup lsp-python
(use-package lsp-python-ms
  :defer t
  :config
  (setq lsp-python-ms-auto-install-server t)
  (add-hook 'python-mode-hook #'lsp-deferred)
  )

;; setup lsp-haskell
(use-package lsp-haskell
  :defer t
  :config
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '())
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp-deferred)
  )

;; setup rust mode and cargo mode
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer))))
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-hook 'rust-mode-hook #'lsp-deferred)


(use-package company-lsp
  :defer t
  :config
  (setq company-lsp-async 1)
  (setq company-lsp-cache-candidates t)
  )

;; setup smartparens
(smartparens-global-mode t)

;; setup company
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq help-at-pt-display-when-idle t)
  (help-at-pt-set-timer)
  )

(use-package company-box
  :hook (company-mode . company-box-mode))

;; setup flyspell
(flyspell-mode t)
(setq flyspell-issue-message-flag nil) ;; improve performance
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)

;; using hippie to enhance company-mode
(setq hippie-expand-try-function-list '(try-expand-debbrev
                                        try-expand-debbrev-all-buffers
                                        try-expand-debbrev-from-kill
                                        try-complete-file-name-partially
                                        try-complete-file-name
                                        try-expand-all-abbrevs
                                        try-expand-list
                                        try-expand-line
                                        try-complete-lisp-symbol-partially
                                        try-complete-lisp-symbol))

;; setup js2-mode
(setq auto-mode-alist
     (append
      '(("\\.js\\'" . js2-mode))
      '(("\\.html\\'" . web-mode))
      auto-mode-alist))

;; setup js2-refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; setup imenu
(defun js2-imenu-make-index ()
  "Function for setting js2-imenu."
  (interactive)
  (save-excursion
    (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
			       ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function 'js2-imenu-make-index)))

;; setup nodejs-repl
(use-package nodejs-repl
  :defer t)

(provide 'init-packages)
;;; init-packages.el ends here
