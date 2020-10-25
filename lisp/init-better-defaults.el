;;; package --- init-better-defaults
;;; Commentary:
;;; Better editing experience here

;;; Code:
;; show parenthesis in pair
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; highlight the current row
(global-hl-line-mode t)
;; using y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; disable ring bell
(setq ring-bell-function 'ignore)
;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; disable auto-save
(setq auto-save-default nil)
;; disable backup
(setq make-backup-files nil)

;; enable recentf-mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; delete selection
(delete-selection-mode t)

(defun indent-buffer()
  "This function indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; disable doubling quotation mark in lisp
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)

;; highlight parentesis when cursor is in side two parenteses.
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parenthesis."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

;; set default directory
(setq default-directory "~/Documents/")

(defun remove-dos-eol ()
  "Replace DOS end-of-lines with Unix end-of-lines."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

;; indent region
(defun indent-region-or-buffer()
  "Indent selected region."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

;; setup web-mode indentation
(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  (setq js2-basic-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

;; c indent
(defun my-c++-mode-hook ()
  (setq c-default-style "linux")
  (setq c-indent-level 4)
  (setq tab-width 4)
  (setq c-basic-offset 4))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)


;; term mouse
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; use python3
(setq python-shell-interpreter "python3")

(defun open-init-package-file()
  "Open init-packages file."
  (interactive)
  (find-file "~/.emacs.d/lisp/init-packages.el"))

(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
