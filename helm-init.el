;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary: This is attempting to be as light a config as possible, so no Magit. Using helm and command line for necessary version control functionality.
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
(setq straight-use-package-by-default t)

;; recentf, savehist, save-place
(recentf-mode t)
(savehist-mode t)
(save-place-mode t)

;; display line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; custom set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; backups
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))


;; UI
(use-package ace-window
  :bind
  ("M-o" . ace-select-window))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (lisp-mode . smartparens-strict-mode)
   (python-mode . smartparens-mode)))

(use-package rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Fonts
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbol Nerd Font"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Completion
;;`helm-execute-persistent-action` on `C-c p`** — This binding is only useful *inside* Helm buffers. Binding it globally means it does nothing outside Helm. Typically this is bound in `helm-map` instead.

(use-package helm
  :bind
  ("C-c p" . helm-execute-persistent-action)
  ("M-x" . helm-M-x) 
  ("C-x C-f" . helm-find-files)
  ("C-x p b" . helm-browse-project) 
  ("C-x p h" . helm-projects-history)
  :custom
  (helm-x-icons-provider 'nerd-icons)
  (completions-detailed t)
  :config
  (helm-mode 1)
  (helm-ff-icon-mode 1)
  (helm-popup-tip-mode 1)
  (setq helm-follow-mode-persistent t))

(use-package helm-ls-git
  :after helm)

;; lsp uses company by default
(use-package company
  :hook
  (after-init . global-company-mode))

(use-package helm-company
  :after helm
  :bind
  ("C-:" . helm-company))

(use-package exec-path-from-shell
    :init
    (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Programming environment
(use-package pyvenv
  :hook
  (python-mode . pyvenv-mode))

(use-package lsp-mode
  :hook ((java-mode . lsp)
	 (python-ts-mode . lsp))
  :commands lsp)

;; `lsp-java`** — `:after lsp` with `:defer t` means it loads after lsp-mode but has no hooks or bindings. It works because `lsp-java` registers itself via `eval-after-load`, but this is subtle.

(use-package lsp-java
  :defer t
  :after lsp)

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer t)

;; LLM
(straight-use-package '(gptel :type git :host github :repo "karthink/gptel"))

(setq gptel-backend (gptel-make-anthropic "Claude"
		      :key #'gptel-api-key
		      :stream t))

;; Postgres
(use-package pg
  :defer t
  :straight (:host github :repo "emarsden/pg-el"))

(use-package pgmacs
  :defer t
  :straight (:host github :repo "emarsden/pgmacs")
  :custom
  (pgmacs-row-list-table-name-width 40))

;; orgmode
;; this is a lightweight config, not my full knowledge base config

;; magit
;; intentionally not using magit in this config. all git operations through helm or shell.

;;; init.el ends here
