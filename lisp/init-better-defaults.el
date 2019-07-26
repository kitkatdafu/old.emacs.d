;; 显示对应括号
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; 高亮当前行
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
;; 禁止备份文件
(setq make-backup-files nil)
;; 启动recentf-mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;; 替换选中部分
(delete-selection-mode t)
;; indent the whole buffer
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

(provide 'init-better-defaults)
