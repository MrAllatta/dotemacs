;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Disable package.el (we use straight.el)
(setq package-enable-at-startup nil)

;; Initial window configuration
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 100) (height . 55)))

;; UI early
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-hl-line-mode 1)


(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      frame-inhibit-implied-resize t
      use-dialog-box nil
      ring-bell-function #'ignore)

(setq load-prefer-newer t)

(load-theme 'modus-vivendi-tritanopia)

(provide 'early-init)

;;; early-init.el ends here


