(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(global-set-key (kbd "C-s") 'helm-swoop-from-isearch)

(global-set-key (kbd "s-/") 'hippie-expand)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(provide 'init-keybindings.el)
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)
