;;; package --- init-packages
;;; Commentary:
;;; Install and setup packages here
;;; Code:
(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize))

(setq package-archives '(
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpa" . "http://melpa.org/packages/")
			 ))

;; Add Packages
(defvar ddy/packages '(
               company
               hungry-delete
	       swiper
	       counsel
	       ivy
               smartparens
	       popwin
	       expand-region
	       iedit
	       flycheck
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
	       haskell-mode
	       flyspell-popup
	       rust-mode
	       all-the-icons
	       magit
	       tuareg
	       org-ref
	       ox-twbs
	       doom-themes
	       evil
	       ivy-bibtex
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
(add-hook 'after-init-hook #'global-flycheck-mode)

;; evil mode
(require 'evil)
(evil-mode 1)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

(add-hook 'write-file-functions
            (lambda ()
               (delete-trailing-whitespace)
               nil))

;; setup swiper/ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

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

;; setup nodejs-repl
(require 'nodejs-repl)

;; setup lsp-mode
(require 'lsp-mode)
(setq lsp-prefer-flymake nil) ;; prefer flycheck
(setq lsp-enable-indentation t)

;; setup lsp-ui
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; setup lsp-python
(require 'lsp-python-ms)
(setq lsp-python-ms-auto-install-server t)
(add-hook 'python-mode-hook #'lsp)

;; setup lsp-haskell
(require 'lsp-haskell)
(setq lsp-haskell-process-path-hie "ghcide")
(setq lsp-haskell-process-args-hie '())
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(require 'company-lsp)
(setq company-lsp-async 1)
(setq company-lsp-cache-candidates t)

;; all the icons
(require 'all-the-icons)

;; setup treemacs
(setq treemacs-width 30)

;; setup js2-mode
(setq auto-mode-alist
     (append
      '(("\\.js\\'" . js2-mode))
      '(("\\.html\\'" . web-mode))
      auto-mode-alist))

;; setup js2-refactor
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;; setup smartparens
(smartparens-global-mode t)

;; setup nyan-mode cat
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)
(setq nyan-wavy-trail t)
(setq nyan-animate-nyancat t)

;; all the icons
(require 'all-the-icons)

;; setup doom themes
(require 'doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
;; Load the theme (doom-one, doom-molokai, etc);
(load-theme 'doom-one-light t)
;; Enable custom neotree theme (all-the-icons must be installed!)
;; (doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)
;; org-mode's native fontification.
(doom-themes-org-config)

;; setup company
(require 'company)
(add-hook 'prog-mode-hook 'company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq help-at-pt-display-when-idle t)
(help-at-pt-set-timer)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; setup popwin
(require 'popwin)
(popwin-mode t)

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

;; setup yasnippet
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; setup rust mode
(add-hook 'rust-mode-hook
         (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)

(provide 'init-packages)
;;; init-packages.el ends here
