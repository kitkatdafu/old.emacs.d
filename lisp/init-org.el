;;; package --- init-org
;;; Commentary:
;;; Setup org
;;; Code:
(setq bibtex-dialect 'biblatex)
(setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf %f"))
(setq org-src-fontify-natively t)

;; org ref
(use-package org-ref-ivy)
(use-package org-ref
  :config
  (setq bibtex-completion-bibliography '("~/Documents/Bibliography/pair.bib"))
  :bind
  (("C-c ]" . 'org-ref-insert-link))
  )

(add-to-list 'org-structure-template-alist
             '("a" "\n\begin{align*}\n\n\end{align*}\n"))
(add-hook  'org-mode-hook  (lambda ()(add-to-list 'org-latex-logfiles-extensions ("tex" "bcf" "xml"))))

(provide 'init-org)
;;; init-ui.el ends here
