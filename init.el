;;; init.el --- init configurations

;;; Commentary:
;;; configuration sections
;;; 1) package management
;;; 2) package installation
;;; 3) customizations for faces and variables
;;; 4) load theme
;;; 5) customize mode line
;;; 6) set global keys
;;; 7) set special global switches by calling functions, set special variables
;;; 8) common hooks for programming modes and non-programming modes
;;; 9) non-common hooks

;;; Code:

;;///////////////////////////////////////////////////////////////////////////////
;; Package Management
;;///////////////////////////////////////////////////////////////////////////////
(defvar DEFAULT-FILL-COLUMN 79)

;;///////////////////////////////////////////////////////////////////////////////
;; Package Management
;;///////////////////////////////////////////////////////////////////////////////
(defvar semantic-symref-filepattern-alist (list))
(defvar emx/USER_REPO_ROOT (getenv "USER_REPO_ROOT"))
(defvar emx/GITHUB_REPO_ROOT (getenv "GITHUB_REPO_ROOT"))
(defvar emx/PYTHON_VIRTUAL_ENV_PATH (concat
                                 emx/USER_REPO_ROOT
                                 (if (eq (substring emx/USER_REPO_ROOT -1 nil) "/") "" "/")
                                 "venv_v"
                                 (getenv "PYTHON_VERSION")))

(package-initialize)
(defun pkg-mgmt()
  "Package management function."
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))
  (eval-when-compile
    (defvar emx/USER_REPO_ROOT (getenv "USER_REPO_ROOT"))
    ;; use-package
    (add-to-list 'load-path (concat emx/USER_REPO_ROOT "/github/use-package"))
    (require 'use-package)
    ;; straight
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
    )
  (setq use-package-always-ensure t)
  )
(pkg-mgmt)
;;///////////////////////////////////////////////////////////////////////////////
;; Install Packages.
;;///////////////////////////////////////////////////////////////////////////////
;;(use-package tree-sitter-langs
;;  :straight t)
;;(use-package tree-sitter
;;  :straight  t
;;  :config
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package rust-mode
  :straight t)
(require 'cl-lib)
;;(use-package gptel
;;  :straight (gptel :type git :host github :repo "karthink/gptel"))
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
(use-package plantuml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(?:pu\\|puml\\)\\'" . plantuml-mode)))
(use-package groovy-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.dsl\\'" . groovy-mode)))
(use-package magit
  :straight t)
(use-package scala-mode
  :straight t
  :interpreter ("scala" . scala-mode)
  :config
  (add-to-list 'semantic-symref-filepattern-alist '(scala-mode "*.scala")))
(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))
(use-package lsp-metals
  :straight t
  :ensure t
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp))
(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
;;(use-package flycheck-yamllint
;;  :ensure t
;;  :defer t
;;  :straight t
;;  :init
;;  (progn
;;    (eval-after-load 'flycheck
;;      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))
(use-package doom-themes
             :straight t)
(use-package multiple-cursors
             :straight t)
(use-package helm
  :straight t
  :init
  (helm-mode 1))
(use-package evil
  :straight t)
(defvar emx/ELPY_ENV_PATH (expand-file-name "elpy/rpc-venv" user-emacs-directory))
(use-package elpy
    :straight t
    :init
;;  (pyvenv-activate emx/PYTHON_VIRTUAL_ENV_PATH)
    (pyvenv-activate emx/ELPY_ENV_PATH)
    (elpy-enable)
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (setq elpy-shell-echo-output nil)
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-timeout 2)
    :hook ((elpy-mode . flycheck-mode)
           (elpy-mode . (lambda ()
                          (set (make-local-variable 'company-backends)
                               '((elpy-company-backend :with company-yasnippet))))))
    :bind
    (:map elpy-mode-map
          ("C-M-n" . elpy-nav-forward-block)
          ("C-M-p" . elpy-nav-backward-block)))
(use-package buftra
    :straight (:host github :repo "humitos/buftra.el"))
(use-package py-pyment
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :config
    (setq py-pyment-options '("--output=numpydoc")))
(use-package py-isort
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-isort-enable-on-save)
    :config
    (setq py-isort-options '("-m=3" "-tc" "-fgw=0" "-ca")))
(use-package py-autoflake
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-autoflake-enable-on-save)
    :config
    (setq py-autoflake-options '("--expand-star-imports")))
(use-package py-docformatter
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-docformatter-enable-on-save)
    :config
    (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))
(use-package blacken
    :straight t
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '88))
(use-package python-docstring
    :straight t
    :hook (python-mode . python-docstring-mode))

(use-package flycheck
             :straight t
             :ensure t
             :init (global-flycheck-mode) (flycheck-display-error-at-point))
(use-package yasnippet :straight t)
;;; swiper need to be installed separately
;;; otherwise, "Autoloading file $HOME/.emacs.d/straight/build/ivy/ivy.elc failed to define function swiper" might happen.
(use-package swiper :straight t)
(when (string-match "28." (emacs-version))
  (use-package ivy
               :straight t
               :diminish
               :bind (("C-s" . swiper)
                      :map ivy-minibuffer-map
                      ("TAB" . ivy-alt-done)
                      ("C-l" . ivy-alt-done)
                      ("C-j" . ivy-next-line)
                      ("C-k" . ivy-previous-line)
                      :map ivy-switch-buffer-map
                      ("C-k" . ivy-previous-line)
                      ("C-l" . ivy-done)
                      ("C-d" . ivy-switch-buffer-kill)
                      :map ivy-reverse-i-search-map
                      ("C-k" . ivy-previous-line)
                      ("C-d" . ivy-reverse-i-search-kill))
               :config
               (ivy-mode 1))
  (use-package  lsp-java
    :straight t)
  (use-package lsp-mode
               :straight t
               :config
               (setq lsp-headerline-arrow
                     #("" 0 1
                       (face #1=(:family "Material Icons" :height 1.0 :inherit lsp-headerline-breadcrumb-separator-face)
                             font-lock-face #1# display
                             (raise 0.0)
                             rear-nonsticky t)))
               :hook (
                      (python-mode . lsp)
                      (java-mode . lsp)
                      (scala-mode . lsp)
                      (lsp-mode . lsp-enable-which-key-integration)
                    )
               :commands lsp)
  (use-package lsp-ui :straight t))
(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'java-mode-hook #'rainbow-delimiters-mode))
(use-package which-key
  :straight t
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))
;; auto-completion
(use-package company
  :straight t
  :diminish company-mode
  :config
  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)  ; completion-at-point-functions
          (company-abbrev company-dabbrev)
          ))
  :hook
  ((java-mode python-mode emacs-lisp-mode shell-script-mode sh-mode scala-mode) . company-mode)
  )
(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package command-log-mode
  :straight t
  :hook (LaTeX-mode 'command-log-mode))
(use-package treemacs
  :straight t
  :config
  (treemacs-git-mode 'deferred))
(use-package anzu
             :straight t)
(use-package awesome-tab
  :straight t
  :config
  (awesome-tab-mode t))
(use-package neotree
  :straight t
  :config
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action))
;; based on json-mode, add function to beautify-json file content and bind to key C-c C-b
;; json-mode rely on yasnippet, so it needs to be end
(defun beautify-json()
  "Beautify-json buffer."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -m json.tool" (current-buffer) t)))

(defun do-beautify-json()
  "Do beautify-json only for json-mode."
  (if (eq major-mode 'json-mode) (beautify-json)))
(add-hook 'before-save-hook 'do-beautify-json)
(use-package json-mode
             :straight t
             :bind (("C-c C-b" . beautify-json))
             )
(defun clean-code-old()
  "Untabify the whole buffer and cleanup whitespaces."
  (interactive)
  (progn
    (mark-whole-buffer)
    (whitespace-cleanup)
    (let ((b (if mark-active (min (point) (mark)) (point-min)))
          (e (if mark-active (max (point) (mark)) (point-max))))
      (untabify b e)
      )
    )
  )
(defun clean-code()
  "Untabify the whole buffer and cleanup whitespaces."
  (interactive)
  (whitespace-cleanup))
(add-hook 'before-save-hook 'clean-code)
;;
;; org mode
;;
(defun emc/org-mode-setup()
  "Setup 'org-mode'."
  (variable-pitch-mode)
  (auto-fill-mode 0)
  (company-mode nil)
  (visual-line-mode 1))
(defun emc/org-mode-visual-fill () "ORG-MODE Visual Fill."
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t
        display-fill-column-indicator-column -1)
  (visual-fill-column-mode 1))
;;; enable org-mode
(defun emc/enable-org-mode() "Enable ORG-MODE."
  ;;;install org-mode package
  (use-package org-mode
    :straight t
    :config
    (org-indent-mode)
    :hook (org-mode . emc/org-mode-setup))
  ;; don't know why this cannot work with :config of use-package org-mode
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))
  (setq org-ellipsis " ▾"
        org-agenda-files
        ;;      '((substitute-in-file-name
        ;;         "${DJ_REPO_ROOT}/redwood/resources/worknote/wnote.org"))
        '(concat emx/USER_REPO_ROOT "/dj/redwood/resources/worknote/wnote.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-todo-keywordso
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (use-package org-bullets
    :straight t
    :hook
    (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
  (use-package visual-fill-column
    :straight t
    :hook (org-mode . emc/org-mode-visual-fill))
  )
(when (or (string-match "27." (emacs-version) (string-match "28." (emacs-version))))
  (if (not (eq system-type 'windows-nt)) (emc/enable-org-mode))
  )
(when (or (string-match "27." (emacs-version) (string-match "28." (emacs-version))))
  (if (not (eq system-type 'windows-nt)) (emc/enable-org-mode))
  )
;; org-mode ends here

;;///////////////////////////////////////////////////////////////////////////////
;; Customized Theme
;;///////////////////////////////////////////////////////////////////////////////
(load-theme 'hacker-2023)

;;///////////////////////////////////////////////////////////////////////////////
;; Customized Mode Line
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
;;///////////////////////////////////////////////////////////////////////////////
(defun install-mode-line-pkgs()
  "Install mode-line dependency packages."
  (use-package doom-modeline
    :straight t)
  (use-package smart-mode-line
    :straight t)
  (use-package mood-line
    :straight t)
;;  (defadvice vc-mode-line (after me/vc-mode-line () activate)
;;  "Strip backend from the VC information."
;;  (when (stringp vc-mode)
;;    (let ((vc-text (replace-regexp-in-string "^ Git." "  " vc-mode)))
;;      (setq vc-mode vc-text))))
  )
(install-mode-line-pkgs)

(defun customize-mode-line()
  "Customize mode line."
  (defgroup w-mode-line nil
    "I want to have a wonderful mode-line: w-mode-line.

  The w-mode-line consists of several segments:
  1. left part: head label - left part segments
  2. right part: right part segments - tail label.
  The segments are separated by separator label."
    :group 'mode-line)

  (defgroup w-palette-faces nil
    "Group for palette"
    :group 'faces)


  (defgroup w-mode-line-faces nil
    "Yes, it includes all w-mode-line faces."
    :group 'w-mode-line
    :group 'faces)

  (defgroup w-mode-line-labels '()
    "Group for mode-line labels."
    :group 'w-mode-line)


  (defcustom w-mode-line-head-label "  "
    "String being used as head label of w-mode-line."
    :type 'string
    :group 'w-mode-line-labels)

  (defcustom w-mode-line-tail-label "  "
    "String being used as head label of w-mode-line."
    :type 'string
    :group 'w-mode-line-labels)

  (defface w-mode-line-head
    '((t (:inherit default :weight normal)))
    "Face for head of w-mode-line."
    :group 'w-mode-line-faces
    )

  (defface w-mode-line-head-modified
    '((t (:inherit default :weight normal)))
    "Face for head of w-mode-line."
    :group 'w-mode-line-faces
    )

  (defface w-mode-line-encoding
    '((t (:inherit default :weight normal)))
    "Face for encoding segment of w-mode-line."
    :group 'w-mode-line-faces
    )

  (defface w-mode-line-tail
    '((t (:inherit default :weight normal)))
    "Face for encoding segment of w-mode-line."
    :group 'w-mode-line-faces
    )

  (defface w-mode-line-tail-modified
    '((t (:inherit default :weight normal)))
    "Face for encoding segment of w-mode-line."
    :group 'w-mode-line-faces
    )

  ;;; palette faces
  ;;; TODO: update docstring
  (defface w-theme-green
    '((t (:inherit default :weight normal)))
    "Face for encoding segment of w-mode-line."
    :group 'w-mode-line-faces
    )

  ) ;;; customize-mode-line fun ends here
(customize-mode-line)

(defun w-mode-line-seg-head()
  "To return head segment of w-mode-line."
  (propertize
   w-mode-line-head-label
   'face (doom-modeline-face
          (if (buffer-modified-p)
              'w-mode-line-head-modified 'w-mode-line-head)
          'w-mode-line-head )))

(w-mode-line-seg-head)

(defun w-mode-line-seg-tail()
  "To return tail segment of w-mode-line."
  (propertize
   w-mode-line-tail-label
   'face (doom-modeline-face
          (if (buffer-modified-p)
              'w-mode-line-tail-modified 'w-mode-line-tail)
          'w-mode-line-tail )))

(w-mode-line-seg-tail)

(defun w-mode-line-seg-buffer-file-name()
  "To return buffer file name if only one window."
  (if (window-full-width-p)
      (buffer-file-name))
  )

(w-mode-line-seg-buffer-file-name)

(defvar-local w-mode-line-vcs-icon nil)
(defun w-mode-line-set-vcs-icon(icon face)
  "Show ICON on mode-line by FACE."
  (propertize icon
              'face face))
(defun w-mode-line-update-vcs-icon (&rest _)
  "Update icon of vcs state in mode-line."
  (setq w-mode-line-vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state (file-local-name buffer-file-name) backend)))
            (cond ((eq state 'unregistered)
                   (w-mode-line-set-vcs-icon "U" "font-lock-comment-face"))
                  ((eq state 'up-to-date)
                   (w-mode-line-set-vcs-icon " " "success"))
                  ((eq state 'edited)
                   (w-mode-line-set-vcs-icon "" "warning"))
                  ((eq state 'added)
                   (w-mode-line-set-vcs-icon "" "font-lock-builtin-face"))
                  ((memq state '(missing removed))
                   (w-mode-line-set-vcs-icon "" "warning"))
                  ((eq state 'conflict)
                   (w-mode-line-set-vcs-icon "" "font-lock-warning-face"))
                  ((eq state 'needs-update)
                   (w-mode-line-set-vcs-icon "" "font-lock-variable-name-face"))
                  ((eq state 'needs-merge)
                   (w-mode-line-set-vcs-icon "" "separator-line"))
                  ((eq state 'ignored)
                   (w-mode-line-set-vcs-icon "◌" "font-lock-comment-face"))
                  (t
                   (w-mode-line-set-vcs-icon " " "vertical-border"))
                  )))))
(add-hook 'find-file-hook #'w-mode-line-update-vcs-icon)
(add-hook 'after-save-hook #'w-mode-line-update-vcs-icon)
(add-hook 'window-state-change-hook #'w-mode-line-update-vcs-icon)

(advice-add #'vc-refresh-state :after #'w-mode-line-update-vcs-icon)
(defun w-mode-line-vcs()
  "Show vcs."
  (when-let ((icon w-mode-line-vcs-icon)
             (vc-text (replace-regexp-in-string "^ Git." "" vc-mode)))
    (concat " (" vc-text " " icon ") ")))

;; TODO: Add Emacs version information on right-side of w-mode-line
(defun w-mode-line()
  "Enable w-mode-line."
  (interactive  nil)
  (setq-default mode-line-buffer-identification
                sml/mode-line-buffer-identification)
  (setq-default mode-line-format
                '((:eval
                   (mood-line--format
                    (format-mode-line
                     '(" "
                       (:eval (w-mode-line-seg-head))
                       (:eval (sml/generate-modified-status))
                       (:eval (doom-modeline--buffer-name))
                       ;;              (vc-mode vc-mode)
                       (:eval (w-mode-line-vcs))
                       (:eval (doom-modeline-segment--buffer-position))
                       (:eval (doom-modeline-segment--buffer-size))
                       "%e"
                       (:eval (w-mode-line-seg-buffer-file-name))
                       ))

                    (format-mode-line
                     '((:eval (doom-modeline-segment--checker))
                       (:eval (doom-modeline-segment--lsp))
                       (:eval (doom-modeline-segment--buffer-encoding))
                       (:eval (doom-modeline-segment--major-mode))
                       (:eval (w-mode-line-seg-tail))
                       " "))))))
  )
;;///////////////////////////////////////////////////////////////////////////////
;; Set Global Keys
;;///////////////////////////////////////////////////////////////////////////////
(defun set-global-keys()
  "To set global keys."
  (global-set-key (kbd "C-c a") 'lsp-execute-code-action)
  ;; use C-c m as prefix for magit
  (global-set-key (kbd "C-c m b") 'magit-blame)
  ;;; C-c C-b bind to beautify-json
  (global-set-key (kbd "C-c c") 'customize)
  (global-set-key (kbd "C-c d") 'kill-line)
  (global-set-key (kbd "C-c e") 'lsp-treemacs-errors-list)
  (global-set-key (kbd "C-c E") 'flycheck-list-errors)
  (global-set-key (kbd "C-c f") 'lsp-format-buffer)
  (global-set-key (kbd "C-c F") 'lsp-java-organize-imports)
  (global-set-key (kbd "C-c h") 'windmove-left)
  (global-set-key (kbd "C-c j") 'windmove-right)
  (global-set-key (kbd "C-c q") 'magit-blame-quit)
  (global-set-key (kbd "C-c r") 'tab-bar-close-tab)
  (global-set-key (kbd "C-c n") 'neotree-toggle)
  (global-set-key (kbd "C-c r") 'lsp-rename)

  ;; use C-c l as prefix for lsp commands
  (global-set-key (kbd "C-c l d s") 'lsp-describe-session)

  ;; use C-c s as prefix for smerge commands
  (global-set-key (kbd "C-c s p") 'smerge-prev)
  (global-set-key (kbd "C-c s n") 'smerge-next)
  (global-set-key (kbd "C-c s k h") 'smerge-keep-upper)
  (global-set-key (kbd "C-c s k l") 'smerge-keep-lower)
  (global-set-key (kbd "C-c s k a") 'smerge-keep-all)

  (global-set-key (kbd "C-c t") 'treemacs)
  (global-set-key (kbd "C-c u") 'untabify)
  (global-set-key (kbd "C-c k") 'kill-whole-line)
  (global-set-key (kbd "C-c v") 'vc-annotate)
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)
  (global-set-key (kbd "C-c x") 'whitespace-mode)
  (global-set-key (kbd "C-c z") 'undo-redo)
  (global-set-key (kbd "C-c (") 'indent-region)
  (global-set-key (kbd "C-c =") 'maximize-window)
  (global-set-key (kbd "C-c .") 'xref-find-references)
  (global-set-key (kbd "C-c <up>") 'windmove-up)
  (global-set-key (kbd "C-c <down>") 'windmove-down)
  (global-set-key (kbd "C-c C-n") 'global-line-numbers-mode)


  (global-set-key (kbd "M-n") 'tab-bar-new-tab)
  (global-set-key (kbd "M-o") 'lsp-ui-imenu)
  (global-set-key (kbd "M-<up>") 'tab-previous)
  (global-set-key (kbd "M-<down>") 'tab-next)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; rebind global keys to emacs extensions keys
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x c C-b") 'helm-mini)
  )
(set-global-keys)
;;///////////////////////////////////////////////////////////////////////////////
;; Set Global Switch
;;///////////////////////////////////////////////////////////////////////////////
;; global switch
(defun global-switch()
  "Set global switch."
  ;; https://github.com/jorgenschaefer/elpy/issues/1729
  (set-language-environment "UTF-8")
  (turn-on-auto-revert-mode)
  (winner-mode)
  (global-tab-line-mode t)
  (set-face-attribute 'default (selected-frame) :height 200)
  (highlight-indentation-mode)
  ;; how to make this work? why .bash_aliases doesn't work
  (add-to-list 'auto-mode-alist '("\\(?:.\\(?:\\(?:zsh\\|bash\\|csh\\|vim\\)\\(?:rc\\)?\\)\\|modules\\)\\(?:\\.[^/]+\\)?\\'" . shell-script-mode))
  (add-to-list 'semantic-symref-filepattern-alist '(xml-mode "*.xml"))
  (add-to-list 'semantic-symref-filepattern-alist '(nxml-mode "*.xml"))
  (add-to-list 'semantic-symref-filepattern-alist '(json-mode "*.json"))
  (setq tab-bar-separator "  "
        tab-line-separator "  "
        tab-bar-button-margin '(5 . 2)
        tags-file-name "~/.emacs.d/TAGS"
        custom-file (concat emx/GITHUB_REPO_ROOT "/emacs-config/custom.el")
        mode-line-end-spaces nil)
  (load custom-file)
  (setq indent-tabs-mode nil)
  (set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ?│)))
(global-switch)
;;///////////////////////////////////////////////////////////////////////////////
;; Hooks for programming language modes and other modes
;;///////////////////////////////////////////////////////////////////////////////
(defvar prog-modes "shell-script java python")
(defvar non-prog-modes "org term eshell treemacs neotree")
(defun prog-env-hook()
  "Programming environment features."
  (display-line-numbers-mode)
  (auto-fill-mode)
  (global-display-fill-column-indicator-mode 1)
  (highlight-indentation-mode)
  (setq show-trailing-whitespace t)
  )
(defun non-prog-env-hook()
  "Non-programming environment features."
  (display-line-numbers-mode t)
  (setq show-trailing-whitespace nil))

(defun append-suffix (suffix phrases)
  "Append SUFFIX to each of PHRASES."
  (mapcar #'(lambda (phrase) (concat phrase suffix)) phrases))

(defun symbols-from-strings (strings)
  "Given a list of STRINGS, get their symbol values."
  (mapcar #'intern strings))

(defun multiple-mode-add-hook (modes hook)
  "Given a list of x-mode-hook symbols in MODES, add the HOOK to them."
  (mapc (lambda (mode) (add-hook mode hook)) modes))

(defun setup-env (mode-names hook)
  "Setup environment by adding HOOK to MODE-NAMES."
  (let ((modes (symbols-from-strings
                (append-suffix "-mode-hook" (split-string mode-names)))))
    (multiple-mode-add-hook modes hook)))

(setup-env prog-modes 'prog-env-hook)
(setup-env non-prog-modes 'non-prog-env-hook)

(defvar JAVA-FILL-COLUMN 120)
(defvar PYTHON-FILL-COLUMN 79)

(defun non-common-hooks()
  "Modes-customization.
Display 'fill column' indicator and dynamically change 'fill-column' value with
different modes."
  (add-hook 'java-mode-hook
            (lambda()
              (progn
                (setq fill-column JAVA-FILL-COLUMN)
                (setq indent-tabs-mode nil)
                )
              ))
  (add-hook 'python-mode (lambda () (setq fill-column PYTHON-FILL-COLUMN)))
  (add-hook 'org-mode (lambda () (setq fill-column -1)))
  (add-hook 'emacs-lisp-mode-hook (lambda() (setq fill-column -1)))
  (add-hook 'rust-mode-hook (lambda() (prettify-symbols-mode)))
)
(non-common-hooks)

;;; do mode-line at the last step to avoid conflict with custom configuration or overrided by custom configurations.
(w-mode-line)
(provide 'init)
;;; init.el ends here
