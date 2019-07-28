(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "M-s i") 'counsel-imenu)

(global-set-key (kbd "s-/") 'hippie-expand)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "M-s o") 'occur-dwim)

(js2r-add-keybindings-with-prefix "C-c C-m")

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "M-s e") 'iedit-mode)

(global-set-key (kbd "H-w") #'aya-create)
(global-set-key (kbd "H-y") #'aya-expand)

(provide 'init-keybindings.el)
;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)