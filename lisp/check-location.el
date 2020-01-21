(call-process "systemsetup" nil "timezone" nil "-gettimezone")
(defun buffer-string* (buffer)
  (with-current-buffer buffer
    (buffer-string)))
(setq mrr-timezone (buffer-string* "timezone"))
(setq am-i-in-china t)
(unless (string= mrr-timezone "Time Zone: Asia/Shanghai\n") (setq am-i-in-china nil))
(kill-buffer "timezone")

(provide 'check-location)
