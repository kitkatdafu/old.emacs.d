;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
;; 关闭文件滑动控件
(when (display-graphic-p)
    (scroll-bar-mode -1))
;; 显示行号
(global-linum-mode t)
;; 关闭启动帮助画面
(setq inhibit-splash-screen t)
;; 更改显示字体
(setq FONT "Iosevka-15")
(set-face-attribute 'default nil :font FONT)
(set-frame-font FONT nil t)
;; transparent titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; emacs开启时候全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(provide 'init-ui)
