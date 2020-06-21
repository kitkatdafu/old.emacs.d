;;; easypanol.el --- Easy Spanish characters from US keyboard
;;; Commentary:
;; This package provide an easy way to enter Spanish characters from US keyboard.
;; Also allows you to find definition of Spanish word from RAE dictionary.

;;; Code:
;; lower case vowels; vocables en minúsculas
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

;; diéresis
(global-set-key (kbd "C-\" u") (lambda () (interactive) (insert "ü")))
(global-set-key (kbd "C-\" u") (lambda () (interactive) (insert "Ü")))

;; eñe
(global-set-key (kbd "C-' n") (lambda () (interactive) (insert "ñ")))
(global-set-key (kbd "C-' N") (lambda () (interactive) (insert "Ñ")))

;; punctuation; puntuación
(global-set-key (kbd "C-' !") (lambda () (interactive) (insert "¡")))
(global-set-key (kbd "C-' ?") (lambda () (interactive) (insert "¿")))

(defvar accent-char-mapping
  '(?a ?á ?e ?é ?i ?í ?o ?ó ?u ?ú ?ú ?ü ?U ?Ü ?A ?Á
       ?E ?É ?I ?Í ?O ?Ó ?U ?Ú ?n ?ñ ?N ?Ñ ?! ?¡ ?? ?¿))

(defun accent-char-before ()
  (interactive)
  (when (looking-back (rx (or (seq (any ?a ?e ?i ?o ?u
					?A ?E ?I ?O ?U
					?n ?N ?? ?!)
				   "''")
			      "ú'"))
		      (- (point) 3))
    (let ((char (char-after (match-beginning 0))))
      (delete-region (match-beginning 0)
		     (match-end 0))
      (insert (plist-get accent-char-mapping char)))))

(define-minor-mode accent-char-mode
  "Type accented char."
  :lighter ""
  :global t
  (if accent-char-mode
      (add-hook 'post-self-insert-hook #'accent-char-before)
    (remove-hook 'post-self-insert-hook #'accent-char-before)))

(defvar dle-rae-url "https://dle.rae.es/")
(defvar dle-rae-buffer " *dle-rae*")

(defun dle-rae-query (word)
  (set-buffer (url-retrieve-synchronously (concat dle-rae-url word)))
  (let ((results))
    (while (re-search-forward "<p class=\"j\"[^>]*>\\(.*?\\)</p>" nil t)
      (push (concat (string-trim (replace-regexp-in-string "<[^<]*>" "" (match-string 0))) "\n") results))
    results))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (with-current-buffer (get-buffer-create dle-rae-buffer) (erase-buffer))
  (while list
    (with-current-buffer (get-buffer-create dle-rae-buffer)
      (insert (car list)))
    (setq list (cdr list)))
  )

(setq posframe--timeout-timer 20)
(global-set-key (kbd "<f9>") (lambda () (interactive)
			       (print-elements-of-list (reverse (dle-rae-query (downcase (thing-at-point 'word 'no-properties)))))
			       (when (posframe-workable-p)
				 (posframe-show dle-rae-buffer
						:font "Times New Roman"
						:background-color "black"
						:foreground-color "white"
						:timeout 10
						:right-fringe 44
						:x-pixel-offset 3000
						:lines-truncate 1
						:max-width 100
						:max-width 100
						)
				 )))
;; decir
;; hola
(accent-char-mode)
(provide 'easypanol)
;;; easypanol.el ends here
