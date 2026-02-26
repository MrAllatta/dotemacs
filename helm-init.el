;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package integration with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; UI
(use-package modus-themes)

(use-package ace-window
  :bind
  ("M-o" . ace-select-window))

;; Fonts
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbol Nerd Font"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package helm
  :init
  (helm-mode 1)
  (helm-ff-icon-mode 1)
  (helm-popup-tip-mode 1)
  :bind
  ("C-c p" . helm-execute-persistent-action)
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x g" . helm-browse-project)
  ("C-x p h" . helm-projects-history)
  :custom
  (helm-x-icons-provider 'nerd-icons)
  (completions-detailed 1)
  :config
  (setq helm-follow-mode-persistent t))

(use-package helm-ls-git
  :after helm)

;; Programming environment
(use-package pyvenv)

(use-package helm-company
  :after helm
  :bind
  ("C-:" . helm-company))

;; lsp uses company by default
(use-package company
  :init
  (require 'helm-company)
  :hook
  (after-init . global-company-mode))

(use-package lsp-mode
  :hook ((java-mode . lsp)        ; Enable LSP for Java
         (python-mode . lsp)      ; Enable LSP for Python
         (javascript-mode . lsp)) ; Add more languages as needed
  :commands lsp)

(use-package lsp-java
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp)
  (setq lsp-auto-guess-root t))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package smartparens
  :init
  (smartparens-global-mode 1))

(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode 1))

(use-package gptel
  :config
  (setq gptel-model 'claude-opus-4-6)
  (setq gptel-backend (gptel-make-anthropic "Claude"
			:stream t :key (getenv "ANTHROPIC_API_KEY"))))
(require 'gptel-org)

(use-package pg
  :straight (:host github :repo "emarsden/pg-el"))

(use-package pgmacs
  :straight (:host github :repo "emarsden/pgmacs")
  :custom
  (pgmacs-row-list-table-name-width 40))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-names-using-follow '("Actions" "Git status")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
