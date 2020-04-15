;;; package --- Summary
;;; Commentary: Chen
;;; Code:
(require 'thingatpt) ;; to use thing at point
(require 'subr-x)

(setq content "printf(\"Variable x = %d\\n\", x)")

(defun qprint ()
            "Get current line."
            (interactive)
	    (with-current-buffer (current-buffer) (insert content))
	    )

(global-set-key (kbd "C-x C-a") 'qprint)

(provide 'quick-print)
;;; quick-print ends here
