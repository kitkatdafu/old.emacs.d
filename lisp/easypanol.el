;;; easypanol.el --- Easy Spanish characters from US keyboard
;;; Commentary:
;; This package provide an easy way to enter Spanish characters from US keyboard.

;;; Code:
;; lower case vowels; vocables en minúscula¿s
(global-set-key (kbd "C-' a") (lambda () (interactive) (insert "á")))
(global-set-key (kbd "C-' e") (lambda () (interactive) (insert "é")))
(global-set-key (kbd "C-' i") (lambda () (interactive) (insert "í")))
(global-set-key (kbd "C-' o") (lambda () (interactive) (insert "ó")))
(global-set-key (kbd "C-' u") (lambda () (interactive) (insert "ú")))

;; lower case vowels; vocables en minúsculas
(global-set-key (kbd "C-' A") (lambda () (interactive) (insert "Á")))
(global-set-key (kbd "C-' E") (lambda () (interactive) (insert "É")))
(global-set-key (kbd "C-' I") (lambda () (interactive) (insert "Í")))
(global-set-key (kbd "C-' O") (lambda () (interactive) (insert "Ó")))
(global-set-key (kbd "C-' U") (lambda () (interactive) (insert "Ú")))

;; eñe
(global-set-key (kbd "C-' n") (lambda () (interactive) (insert "ñ")))
(global-set-key (kbd "C-' N") (lambda () (interactive) (insert "Ñ")))

;; punctuation; puntuación
(global-set-key (kbd "C-' !") (lambda () (interactive) (insert "¡")))
(global-set-key (kbd "C-' ?") (lambda () (interactive) (insert "¿")))

(provide 'easypanol)
;;; easypanol.el ends here
