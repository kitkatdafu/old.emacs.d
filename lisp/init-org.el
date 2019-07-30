;; org-mode语法高亮
(require 'org)
(setq org-src-fontify-natively t)
(set-language-environment "UTF-8")
(add-hook 'org-mode-hook truncate-lines)

(provide 'init-org)
