;; org-mode语法高亮
(require 'org)
(setq org-src-fontify-natively t)
(setq org-list-allow-alphabetical t)
(set-language-environment "UTF-8")
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;; (setq org-agenda-files '("~/gtd/gtd.org"))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

;; (add-to-list 'org-structure-template-alist '("TITLE" "#+TITLE: 
;; #+AUTHOR: Yi Chen
;; #+EMAIL: ychen878@wisc.edu
;; #+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.mrr1vfe.io/docs/org.css\"/>"))

;; (add-to-list 'org-structure-template-alist '("begin" "\\begin{align*}

;; \\end{align*}
;; "))


(provide 'init-org)
