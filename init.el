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
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; setup helm
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x r p") 'helm-projects-history)
(global-set-key (kbd "C-s") 'swiper-helm)
(require 'helm-ls-git)

;; setup eglot
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

;; setup cargo
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; Better Defaults -----------------------------------------------------

(require 'hungry-delete)
(global-hungry-delete-mode)
;; smart parenthesis
(smartparens-global-mode t)
;; show parenthesis in pair
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; highlight the current row
(global-hl-line-mode t)
;; using y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; turn off welcome screen
(setq inhibit-splash-screen t)
;; disable ring bell
(setq ring-bell-function 'ignore)
;; Find Executable Path on macOS
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; disable auto-save
(setq auto-save-default nil)
;; disable backup
(setq make-backup-files nil)
;; delete selection
(delete-selection-mode t)
;; highlight parentesis when cursor is in side two parenteses.
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parenthesis."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))



;;; GUI -----------------------------------------------------------------
;; turn off tool bar
(tool-bar-mode -1)
;; turn off scroll bar
(when (display-graphic-p)
  (scroll-bar-mode -1))
;; display line number
(global-linum-mode t)
;; setup font
(defvar editor-font "Iosevka-18")
(set-face-attribute 'default nil :font editor-font)
(set-frame-font editor-font nil t)
;; setup transparent title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; setup all the icons
(use-package all-the-icons)
;; setup nyan-mode cat
(use-package nyan-mode
  :config
  (nyan-mode)
  (nyan-start-animation)
  (setq nyan-wavy-trail t)
  (setq nyan-animate-nyancat t)
  )
;; setup doom themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-monokai-pro t)
  ;; Corrects and improves org-mode's native fontification.
  (doom-themes-org-config))
;; setup doom modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;; setup ligatures
(let ((ligatures `((?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                   (?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                   (?*  . ,(regexp-opt '("*>" "***" "*/")))
                   (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
                                         "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                         "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                   (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
                   (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                   (?!  . ,(regexp-opt '("!==" "!!" "!=")))
                   (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                   (?&  . ,(regexp-opt '("&&&" "&&")))
                   (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                   (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                   (?+  . ,(regexp-opt '("+++" "+>" "++")))
                   (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
                   (?\{ . ,(regexp-opt '("{|")))
                   (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
                   (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                   (?\; . ,(regexp-opt '(";;")))
                   (?_  . ,(regexp-opt '("_|_" "__")))
                   (?\\ . ,(regexp-opt '("\\" "\\/")))
                   (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                   (?$  . ,(regexp-opt '("$>")))
                   (?^  . ,(regexp-opt '("^=")))
                   (?\] . ,(regexp-opt '("]#"))))))
  (dolist (char-regexp ligatures)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package composite
  :hook (prog-mode . auto-composition-mode)
  :init (global-auto-composition-mode -1))
