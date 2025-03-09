;; (require 'package)
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                         ("org"   . "https://orgmode.org/elpa/")
;;                         ("gnu"   . "https://elpa.gnu.org/packages/")))
;; (package-initialize)
;; (unless package-archive-contents
;;  (package-refresh-contents))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setenv "CC" "/bin/gcc")

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Display time in the mode-line
(display-time-mode 1)
(setq display-time-format "%a %b %d %I:%M%p")

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Use 'y' or 'n' instead of 'yes' or 'no' in prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Sentences end with a single space instead of two
(setq sentence-end-double-space nil)


(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-c b") 'consult-buffer)
(global-set-key (kbd "M-x") 'execute-extended-command)

(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)

(global-set-key (kbd "C-c C-c") 'compile)  ;; Change compile keybinding

(use-package modus-themes
; :ensure t
  :config
;;  Ensure the package is fully loaded before applying theme
  (require 'modus-themes)

;;  Load preferred theme at startup
  (modus-themes-select 'modus-operandi)

;;  Improve readability and contrast
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-scale-headings t
        modus-themes-org-blocks 'gray-background))

;; Toggle between themes with a keybinding
(defun toggle-modus-themes ()
  "Toggle between Modus Operandi (light) and Modus Vivendi (dark)."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (modus-themes-select 'modus-vivendi)
    (modus-themes-select 'modus-operandi)))

(global-set-key (kbd "<f5>") 'toggle-modus-themes)

(use-package ivy
; :ensure t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-wrap t))

(use-package consult
					; :ensure t
  
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-c h" . consult-history)))

(use-package company
; :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.1
        company-tooltip-align-annotations t))

(use-package markdown-mode
; :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
;;  Use Pandoc for markdown commands
  (setq markdown-command "pandoc"
;;        Better code block highlighting
        markdown-fontify-code-blocks-natively t)
  :hook
;;  Enable soft-wrapping in markdown buffers
  (markdown-mode . visual-line-mode))

;; Ensure Tree-Sitter is installed and configured
(setq treesit-language-source-alist
      '((python      "https://github.com/tree-sitter/tree-sitter-python")
        (yaml        "https://github.com/ikatyang/tree-sitter-yaml")
					       (latex       "https://github.com/latex-lsp/tree-sitter-latex")
        (java        "https://github.com/tree-sitter/tree-sitter-java")
					       (typescript  ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript"))
        (javascript  "https://github.com/tree-sitter/tree-sitter-javascript")
        (markdown    "https://github.com/ikatyang/tree-sitter-markdown")
        (org         "https://github.com/milisims/tree-sitter-org")))

(defun ensure-treesitter-grammars-installed ()
  "Ensure all required Tree-Sitter grammars are installed."
  (message "🔍 Tree-sitter: Checking installed grammars...")
  (dolist (lang '(python yaml java javascript markdown org))
    (if (treesit-language-available-p lang)
        (message "✅ Tree-sitter: %s already installed." lang)
      (progn
        (message "⚠️ Tree-sitter: Installing %s..." lang)
        (treesit-install-language-grammar lang)))))

(add-hook 'after-init-hook #'ensure-treesitter-grammars-installed)

;; Use Tree-Sitter modes where available
(setq major-mode-remap-alist
      '((python-mode        . python-ts-mode)
        (yaml-mode          . yaml-ts-mode)
;	(latex-mode         . latex-ts-mode)
        (java-mode          . java-ts-mode)
;	(typescript-mode    . typescript-ts-mode)
        (js-mode            . javascript-ts-mode)
;        (markdown-mode      . markdown-ts-mode)
	))

(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
         (js-mode . lsp)
         (rust-mode . lsp)
         (go-mode . lsp)
         (c-mode . lsp))
  :config
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting t))

(defun backup-init-file ()
  "Backup Emacs configuration before reloading."
  (let ((backup-file "~/.emacs.d/init.bak"))
    (copy-file user-init-file backup-file t)))

(defun restore-init-file ()
  "Restore the last working Emacs configuration."
  (interactive)
  (let ((backup-file "~/.emacs.d/init.bak"))
    (if (file-exists-p backup-file)
        (copy-file backup-file user-init-file t)
      (message "No backup found!"))))

(add-hook 'before-init-hook 'backup-init-file)

(setq debug-on-error t)

(defun log-emacs-errors ()
  "Log errors during startup."
  (let ((log-file "~/.emacs.d/emacs-errors.log"))
    (with-temp-file log-file
      (insert (format "Last Emacs error log: %s\n\n" (current-time-string)))
      (insert (with-output-to-string (backtrace))))))

(add-hook 'emacs-startup-hook
          (lambda () (when debug-on-error (log-emacs-errors))))

(setq use-package-always-defer t)
(use-package ivy :defer 1 :config (ivy-mode 1))
(use-package company :hook (prog-mode . company-mode))
(use-package lsp-mode :hook ((python-mode . lsp) (js-mode . lsp)))

(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

(defun consult-unified-search ()
  "Unified search interface for LSP symbols, project files, and ripgrep."
  (interactive)
  (let ((choice (completing-read "Search Type: " '("LSP Symbols" "Project Files" "Ripgrep"))))
    (cond
     ((string= choice "LSP Symbols") (call-interactively 'consult-lsp-symbols))
     ((string= choice "Project Files") (call-interactively 'consult-find))
     ((string= choice "Ripgrep") (call-interactively 'consult-ripgrep)))))

(global-set-key (kbd "C-c u") 'consult-unified-search)

;; (use-package org
;;   :bind (("C-c a" . org-agenda)   Open Org-Agenda
;;          ("C-c l" . org-store-link) Store link
;;          ("C-c c" . org-capture)   Capture new note
;;          ("C-c b" . org-switchb)   Switch Org buffer
;;          ("C-c d" . org-deadline)  Set deadline
;;          ("C-c s" . org-schedule)  Schedule task
;;          ("C-c ." . org-time-stamp) Insert timestamp
;;          ("C-c ," . org-priority))
;;   :config
;;   (setq org-agenda-files (directory-files-recursively "~/orgfiles/" "\\.org$"))))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c d") 'org-deadline)
(global-set-key (kbd "C-c s") 'org-schedule)
(global-set-key (kbd "C-c .") 'org-time-stamp)
(global-set-key (kbd "C-c ,") 'org-priority)

;(setq org-agenda-files (directory-files-recursively "~/orgfiles/" "\\.org$"))

;;  Set priority
(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "~/orgfiles/tasks.org" "Tasks")
         "* TODO %?\n  %U\n  %a\n")

        ("n" "Note" entry
         (file+headline "~/orgfiles/notes.org" "Notes")
         "* %?\n  %U\n  %a\n")

	("s" "Schedule a Meeting" entry
	 (file+headline "~/orgfiles/schedule.org" "Upcoming Meetings")
	 "* TODO Meeting with %^{Who}\n  SCHEDULED: %^T\n  %?")

	("a" "AI Query" entry
         (file "~/org/ai-queries.org")
         "* %U - %^{Query}\n#+BEGIN_AI\n%?\n#+END_AI\n")

      ("j" "Job Application" entry
       (file+headline "~/projects/teaching-applications/applications.org" "Applications")
       "* %^{Job Title} at %^{School}
                 :PROPERTIES:
                 :Submitted: %U
                 :Follow-Up: %^t
                 :END:
                 - Status: %^{Status|Pending|Submitted|Interview|Offer}
                 - Notes: %?")))

(use-package org-roam
  ; :ensure t
  :init
  (setq org-roam-directory (expand-file-name "~/orgfiles/roam/"))
  (setq org-roam-list-files-commands '(find rg))  ;; Ensure recursive search
  (setq org-roam-dailies-directory "daily/")

  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))

  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r t" . org-roam-tag-add)
         ("C-c r d" . org-roam-dailies-capture-today)
	 ("C-c r b" . org-roam-buffer-toggle)
	 ("C-c r m" . org-roam-capture))
  :config
  ;; Ensure Org-Roam database syncs at startup
  (org-roam-db-autosync-mode)
  (add-hook 'after-init-hook #'org-roam-db-sync)) ;; Ensure database is updated on startup

(setq org-roam-extra-directories '("~/knowledge/"))

(defun my-org-roam-list-files ()
  "Return a list of all Org-Roam files in multiple directories, including subdirectories."
  (let ((org-roam-files
         (append
          (org-roam--list-files-elisp org-roam-directory)
          (org-roam--list-files-elisp (expand-file-name "~/knowledge/")))))
    (delete-dups org-roam-files)))

(advice-add 'org-roam-list-files :override #'my-org-roam-list-files)

(defun ensure-org-roam-files-have-ids ()
  "Ensure every Org-Roam file has a #+title: property and a unique ID."
  (dolist (file (org-roam-list-files))
    (with-temp-buffer
      (insert-file-contents file)
      (let (needs-save)
        ;; Ensure the file has a title
        (unless (re-search-forward "^#\\+title:" nil t)
          (goto-char (point-min))
          (insert "#+title: " (file-name-base file) "\n")
          (setq needs-save t))
        ;; Ensure the file has an Org-Roam ID
        (unless (re-search-forward ":ID:" nil t)
          (goto-char (point-min))
          (insert ":PROPERTIES:\n:ID: " (org-id-new) "\n:END:\n\n")
          (setq needs-save t))
        ;; Save only if changes were made
        (when needs-save
          (write-file file))))))

;; Run this function after database sync to fix missing IDs
(add-hook 'org-roam-db-sync-hook #'ensure-org-roam-files-have-ids)

(setq org-roam-capture-templates
      '(("d" "Default" plain
         "%?"
         :if-new (file+head "%<%Y-%m-%d>-${slug}.org"
                            "#+title: ${title}\n#+date: %U\n")
         :unnarrowed t)

        ("p" "Project" plain
         "* Goals\n%?\n* Notes\n- %U\n- Related: %a\n"
         :if-new (file+head "projects/%<%Y-%m-%d>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :project:\n")
         :unnarrowed t)
	
	("c" "CLI Command Knowledge" plain
	 "* ${title}\n  :PROPERTIES:\n  :ID: %(org-id-new)\n  :END:\n  #+title: ${title}\n  #+filetags: :cli:commands:\n  #+created: %U\n\n** Summary\n${summary}\n\n** Command Syntax\n#+begin_src bash\n${command}\n#+end_src\n\n** Explanation\n${explanation}\n\n** Example\n${example}\n\n** Notes\n${notes}\n\n** Related Commands\n- ${related}\n"
	 :target (file+olp "cli-commands.org" ("CLI Commands"))
	 :unnarrowed t
	 :empty-lines 1
	 :prepend t
	 :kill-buffer t)


  	("m" "Meeting Notes" plain
         "** MEETING with %^{Who} on %^T\n  :PROPERTIES:\n  :ID: %(org-id-new)\n  :END:\n  #+title: Meeting - %^{Who} - %<%Y-%m-%d>\n\n** Agenda\n%?\n\n** Notes\n- "
         :target (file+olp "meetings.org" ("Meetings"))
         :unnarrowed t
         :empty-lines 1
         :prepend t
         :kill-buffer t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(setq org-agenda-files '("~/orgfiles/tasks.org"
                         "~/orgfiles/projects.org"
                         "~/orgfiles/meetings.org"
			 "~/orgfiles/schedule.org"
			 "~/projects/teaching-applications/applications.org"))

(setq org-agenda-custom-commands
      '(("d" "Daily Overview"
         ((agenda "" ((org-agenda-span 'day)))
          (todo "TODO")
          (tags "project")))

        ("w" "Weekly Plan"
         ((agenda "" ((org-agenda-span 'week)))
          (todo "NEXT")))

        ("p" "Project Overview"
         ((tags-todo "project")
          (todo "WAITING")))))

(setq org-deadline-warning-days 7) ;; Show upcoming deadlines 7 days in advance

(require 'org-habit)  ;; Ensure org-habit is loaded
(add-to-list 'org-modules 'org-habit)

(setq org-habit-graph-column 50)  ;; Align habit tracking visuals
(setq org-habit-preceding-days 7) ;; Show past 7 days of habit tracking
(setq org-habit-following-days 7) ;; Show next 7 days of habit tracking

(require 'org)
(require 'org-agenda)
(require 'org-roam)
(require 'org-roam-protocol)
(global-set-key (kbd "C-c r f") 'org-roam-node-find) ;; Ensure Org-Roam loads

(use-package org-roam
  :defer 2 ;; Load 2 seconds after startup
  :config (org-roam-db-autosync-mode))

(use-package magit
;  :ensure t                     ; Ensure Magit is installed
  :commands (magit-status       ; Defer loading until we need magit-status
             magit-init         ; Also defer loading magit-init
             magit-file-dispatch)
  :bind (("C-x g" . magit-status)  ; Keybinding for magit-status
         ("C-x M-g" . magit-status) ; Alt + g as a secondary keybinding
         ("C-x C-g" . magit-dispatch) ; Keybinding for magit-dispatch (for additional commands)
         ("C-x C-g" . magit-file-dispatch)) ; Dispatch commands on files
  :config
  ;; Set up Magit to automatically run some git commands upon initialization
  (setq magit-auto-revert-mode t)    ; Auto-refresh Magit status buffer
  (setq magit-diff-refine-hunk 'all) ; Refine diff display
 ; (setq magit-completing-read-function 'magit-completing-read-default)
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-diff-options '("--color=auto")) ; Add color to diffs
  (setq magit-push-always-verify nil) ; Disable push verification prompts

  ;; Optional: Setup for working with remot8/e repositories
  (setq magit-fetch-arguments '("--prune")) ; Auto prune when fetching
  
  ;; Setup for controlling diffs
  (setq magit-diff-highlight-hunk-region t) ; Highlights diff regions

  ;; Additional optional configurations to improve experience
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "--oneline")))

;; Optional: Automatically load magit when visiting a git repo DOESN'T WORK
;(add-hook 'git-commit-mode-hook
;          (lambda () (magit-mode 1)))

(setq gc-cons-threshold (* 50 1000 1000)) ;; Increase garbage collection threshold
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000)))) ;; Reset after startup
;(debug-on-entry 'treesit-install-language-grammar)

(use-package denote
;  :ensure t
  :custom
  (denote-directory "~/knowledge/") ;; Centralized note storage
  (denote-file-type 'org) ;; Use Org-mode for note-taking
  :bind
  (("C-c n n" . denote) ;; Create a new note
   ("C-c n f" . denote-find-link)
   ("C-c n r" . denote-rename-file))) ;; Find existing notes



(straight-use-package 'gptel)
(setq gptel-api-key (getenv "OPENAI_API_KEY"))
(setq gptel-org-branching-context t)
(global-set-key (kbd "C-c g p" ) 'gptel)
(global-set-key (kbd "C-c g s" ) 'gptel-send)
(global-set-key (kbd "C-c g m" ) 'gptel-menu)
(global-set-key (kbd "C-c S") 'gptel-stop)
;(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
;(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(package-selected-packages
 ;;   '(company consult denote ivy magit markdown-mode modus-themes org-ai
 ;; 	     org-roam))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
 
;; Job application status functions
(defun update-job-status (job-id new-status)
  "Update the status of a job based on the job ID."
  (interactive "sJob ID: \nsNew Status: ")
  (with-current-buffer (find-file-noselect "~/projects/teaching-applications/applications.org")
    (goto-char (point-min))
    (when (re-search-forward (concat ":JobID: " job-id) nil t)
      (org-entry-put (point) "Status" new-status)
      (save-buffer))))


;;  In each note, include sections and versioning details as specified in your framework. You can create templates using the following:

(defun my-job-application-template (type)
  "Create a job application note with specified TYPE."
  (let ((title (read-string "Title: ")))
    (org-roam-capture- :node (org-roam-node-create)
                       :templates (list (list "d" "Job Application" "* TODO %?"
                                              ":PROPERTIES:\n:Version: 1.0\n:END:\n\n** %?" 
                                              "** Next Steps\n- \n** Feedback\n- ")))))


(defun query-status-of-subthreads ()
  "Query the status of subthreads 002-009 in applications.org and log updates under ** Logs."
  (interactive)
  (let ((org-file "~/projects/teaching-applications/applications.org") ; Update with your real file path
        (subthread-ids '("002" "003" "004" "005" "006" "007" "008" "009"))
        (current-date (format-time-string "%Y-%m-%d"))
        (log-heading "** Logs")
        (log-entry "")
        (subthread-status nil))
    
    (with-current-buffer (find-file-noselect org-file)
      (org-mode)
      
      (dolist (id subthread-ids)
        (let ((subthread-name (format "Status of Thread %s" id)))
          ;; Sending query to gptel
          (setq subthread-status (gptel-send (concat "What is the current status of " subthread-name "?")))
          
          ;; Generating log entry
          (setq log-entry (format "*** Updated log %s - %s\n%s\n\n" current-date subthread-name subthread-status))
          
          ;; Navigate to the Logs section
          (goto-char (point-min))
          (search-forward log-heading)
          
          ;; Insert the new log entry
          (insert log-entry)))
      
      ;; Save the changes to the file
      (save-buffer))))


;; Here’s an Emacs Lisp (Elisp) script that extracts resume updates from an Org-mode file and formats them into a structured table. This script will:

;; Parse entries under * Resume Updates
;; Extract Version, ATS Check Status, Submission Method, Last Update Date, and Feedback
;; Display the results in an Org table for easier review.


(defun extract-resume-updates ()
  "Extracts resume updates from the Org file and formats them into a structured Org table."
  (interactive)
  (let ((output-buffer "*Resume Updates Table*")
        (data '()))
    (with-current-buffer (find-file-noselect "resume_tracker.org")  ;; Change to your Org file path
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (when (string= (org-element-property :title hl) "Resume Updates")
            (dolist (sub (org-element-contents hl))
              (when (eq (car sub) 'headline)
                (let* ((version (org-element-property :title sub))
                       (ats-check (extract-property sub "ATS Check Status"))
                       (submission (extract-property sub "Submission Methods"))
                       (last-update (extract-property sub "Last Update Date"))
                       (feedback (extract-property sub "Feedback")))
                  (push (list version ats-check submission last-update feedback) data))))))))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (insert "| Version | ATS Check Status | Submission Method | Last Update | Feedback |\n")
      (insert "|---------+-----------------+-----------------+-------------+----------|\n")
      (dolist (entry (reverse data))
        (insert (format "| %s | %s | %s | %s | %s |\n"
                        (nth 0 entry) (nth 1 entry) (nth 2 entry) (nth 3 entry) (nth 4 entry))))
      (org-mode)
      (switch-to-buffer output-buffer))))

(defun extract-property (headline property)
  "Extracts a property value from a given Org headline content."
  (let ((content (org-element-contents headline)))
    (catch 'result
      (dolist (elem content)
        (when (and (eq (car elem) 'section)
                   (string-match (format "^*** %s: \\(.*\\)$" property)
                                 (org-element-interpret-data elem)))
          (throw 'result (match-string 1 (org-element-interpret-data elem)))))
      "N/A")))  ;; Default if the property isn't found
(put 'upcase-region 'disabled nil)
