;; org-mode语法高亮
(require 'org)
(setq org-src-fontify-natively t)
(setq org-list-allow-alphabetical t)
(set-language-environment "UTF-8")
(add-hook 'org-mode-hook 'toggle-truncate-lines)


(provide 'init-org)
