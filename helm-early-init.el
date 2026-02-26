;;; early-init.el --- Early configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq inhibit-startup-message t)
(global-display-line-numbers-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'modus-vivendi-deuteranopia :no-confirm)

(setq package-enable-at-startup nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(fset 'yes-or-no-p 'y-or-n-p)
;;; early-init.el ends here
