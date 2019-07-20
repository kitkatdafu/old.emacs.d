;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/"))))

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar ddy/packages '(
               ;; --- Auto-completion ---
               company
               ;; --- Better Editor ---
               hungry-delete
	       helm
	       helm-swoop
               smartparens
	       popwin
               ;; --- Major Mode ---
               js2-mode
               ;; --- Minor Mode ---
               nodejs-repl
               exec-path-from-shell
	       nyan-mode
	       eclim
	       company-emacs-eclim
	       ;; --- Themes ---
	       doom-themes
	       ) "Default packages")

(setq package-selected-packages ddy/packages)

(defun ddy/packages-installed-p ()
  (loop for pkg in ddy/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (ddy/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ddy/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; setup flyspell
(flyspell-mode t)
(add-hook 'org-mode-hook 'flyspell-mode)
;; setup popwin
(require 'popwin)
(popwin-mode t)
;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; disable auto-save
(setq auto-save-default nil)
;; setup eclim
(require 'eclim)
(add-hook 'java-mode-hook 'eclim-mode)
(setq eclimd-autostart t)
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
;; setup nodejs-repl
(require 'nodejs-repl)
;; setup js2-mode
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))
;; setup smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
;; setup helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 32)
(setq helm-autoresize-min-height 28)
(helm-mode 1)
;; Locate the helm-swoop folder to your path
(add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")
(require 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop-from-isearch)
;; setup nyan-mode cat
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)
(setq nyan-wavy-trail t)
(setq nyan-animate-nyancat t)
;; setup doom themes
(require 'doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
;; Load the theme (doom-one, doom-molokai, etc);
(load-theme 'doom-dracula t)
;; Enable custom neotree theme (all-the-icons must be installed!)
;; (doom-themes-neotree-config)
;; or for treemacs users
;; (doom-themes-treemacs-config)
;; org-mode's native fontification.
(doom-themes-org-config)
;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
;; 关闭文件滑动控件
(scroll-bar-mode -1)
;; 显示行号
(global-linum-mode t)
;; 关闭启动帮助画面
(setq inhibit-splash-screen t)
;; 更改显示字体
(setq FONT "SF Mono-16")
(set-face-attribute 'default nil :font FONT)
(set-frame-font FONT nil t)
;; transparent titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)
;; 开启全局 Company 补全
(global-company-mode 1)
;; 禁止备份文件
(setq make-backup-files nil)
;; org-mode语法高亮
(require 'org)
(setq org-src-fontify-natively t)
;; 启动recentf-modex
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; 替换选中部分
(delete-selection-mode t)
;; emacs开启时候全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; 显示对应括号
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; 高亮当前行
(global-hl-line-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.05)
 '(company-minimum-prefix-length 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-external-variable ((t (:foreground "dark gray")))))
