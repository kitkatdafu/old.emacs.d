;; org-mode语法高亮
(require 'org)
(setq org-src-fontify-natively t)
(setq org-list-allow-alphabetical t)
(set-language-environment "UTF-8")
(add-hook 'org-mode-hook 'toggle-truncate-lines)

(add-to-list 'org-structure-template-alist '("N" "#+TITLE: 
#+AUTHOR: Yi Chen
#+EMAIL: ychen878@wisc.edu
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.mrr1vfe.io/docs/org.css\"/>"))


(provide 'init-org)
