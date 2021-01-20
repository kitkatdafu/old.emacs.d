;;; package --- init-better-defaults
;;; Commentary:
;;; Better editing experience here
;;; Code:
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
;; disable doubling quotation mark in lisp
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
;; open quick open init-packages.el
(defun open-init-package-file()
  "Open init-packages file."
  (interactive)
  (find-file "~/.emacs.d/lisr/init-packages.el"))
;; set tabwidth
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
