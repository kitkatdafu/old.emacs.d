;;; package --- init-org
;;; Commentary:
;;; This file configure org-mode
;;; Code:

(use-package org
  :defer t
  :init
  (setq org-src-fontify-natively t)
  (setq org-list-allow-alphabetical t)
  (set-language-environment "UTF-8")
  :config
  (setq bibtex-completion-library-path '("~/Documents/bibliography/bibtex-pdfs/"))
  (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  )

(use-package org-ref
  :defer t
  :init
  (setq reftex-default-bibliography '("~/Documents/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/Documents/bibliography/notes.org"
	org-ref-default-bibliography '("~/Documents/bibliography/references.bib")
	org-ref-pdf-directory "~/Documents/bibliography/bibtex-pdfs/")
  (setq org-latex-pdf-process
	'("pdflatex -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"))
  )

(provide 'init-org)
;;; init-org.el ends here
