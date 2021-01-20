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
  (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
  :config
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  )
(provide 'init-org)
;;; init-org.el ends here
