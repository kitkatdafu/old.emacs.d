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


(provide 'init-org)
;;; init-ui.el ends here
