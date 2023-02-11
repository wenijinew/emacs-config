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
(package-initialize)
(defun pkg-mgmt()
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
						   ("org" . "https://orgmode.org/elpa/")
						   ("elpa" . "https://elpa.gnu.org/packages/")))
  (eval-when-compile
	;; use-package
	(add-to-list 'load-path "/repo/egugwen/github/use-package")
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
;; Install Packages
;;///////////////////////////////////////////////////////////////////////////////
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
(use-package elpy
	:straight t
	:bind
	(:map elpy-mode-map
		  ("C-M-n" . elpy-nav-forward-block)
		  ("C-M-p" . elpy-nav-backward-block))
	:hook ((elpy-mode . flycheck-mode)
		   (elpy-mode . (lambda ()
						  (set (make-local-variable 'company-backends)
							   '((elpy-company-backend :with company-yasnippet))))))
	:init
	(elpy-enable)
	:config
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(setq elpy-shell-echo-output nil)
	(setq elpy-rpc-python-command "python3")
	(setq elpy-rpc-timeout 2))
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
	:straight t
	:init
	(setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms1G")))
  (use-package lsp-mode
			   :straight t
			   :config
			   (setq lsp-keymap-prefix "C-c l")
			   (setq lsp-modeline-diagnostics-scope :workspace)
			   (setq lsp-headerline-arrow
					 #(">" 0 1
					   (face #1=(:family "Material Icons" :height 1.0 :inherit lsp-headerline-breadcrumb-separator-face)
							 font-lock-face #1# display
							 (raise 0.0)
							 rear-nonsticky t)))
			   :hook (
					  (python-mode . lsp)
					  (java-mode . lsp)
					  (lsp-mode . lsp-enable-which-key-integration))
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
  ((java-mode python-mode emacs-lisp-mode shell-script-mode) . company-mode)
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
(use-package neotree
  :straight t
  :config
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action))
;; based on json-mode, add function to beautify-json file content and bind to key C-c C-b
;; json-mode rely on yasnippet, so it needs to be end
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
		(e (if mark-active (max (point) (mark)) (point-max))))
	(shell-command-on-region b e
							 "python -m json.tool" (current-buffer) t)))
(use-package json-mode
			 :straight t
			 :bind (("C-c C-b" . beautify-json))
			 )
;;
;; org mode
;;
(defun emc/org-mode-setup()
  "Setup 'org-mode'."
  (variable-pitch-mode)
  (auto-fill-mode 0)
  (company-mode nil)
  (visual-line-mode 1))
(defun emc/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
		visual-fill-column-center-text t
		display-fill-column-indicator-column -1)
  (visual-fill-column-mode 1))
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
	  ;;	  '((substitute-in-file-name
	  ;;		 "${DJ_REPO_ROOT}/redwood/resources/worknote/wnote.org"))
	  '("/repo/egugwen/dj/redwood/resources/worknote/wnote.org")
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
;; org-mode ends here
;;///////////////////////////////////////////////////////////////////////////////
;; Customizations
;;///////////////////////////////////////////////////////////////////////////////
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-overstrike ((t (:inherit font-lock-type-face :weight bold))))
 '(Man-underline ((t (:inherit font-lock-keyword-face :underline t))))
 '(company-template-field ((t (:background "#b7b7ff" :foreground "#181522"))))
 '(company-tooltip ((t (:background "#30304e"))))
 '(company-tooltip-annotation ((t (:foreground "#e3cfcf"))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :foreground "#c2f0c2"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-common :foreground "#ff9d9d"))))
 '(company-tooltip-selection ((t (:extend t :background "#181522"))))
 '(custom-comment ((t (:background "#181522" :foreground "#d1ccba"))))
 '(doom-modeline-project-dir ((t (:inherit (w-mode-line-head bold)))))
 '(error ((t (:foreground "tomato" :weight bold))))
 '(fill-column-indicator ((t (:inherit font-lock-comment-face))))
 '(git-gutter:deleted ((t (:inherit default :foreground "#e80000" :weight bold))))
 '(header-line ((t (:background "#8585ff" :foreground "#231f32" :inverse-video t :box (:line-width (1 . -1) :color "red" :style released-button)))))
 '(helm-buffer-directory ((t (:inherit font-lock-doc-face))))
 '(helm-ff-directory ((t (:inherit font-lock-doc-face))))
 '(helm-ff-dotted-directory ((t (:extend t :foreground "turquoise"))))
 '(helm-ff-file-extension ((t (:inherit font-lock-type-face))))
 '(helm-selection ((t (:inherit hl-line))))
 '(helm-source-header ((t (:extend t :background "#181522" :foreground "#b7b7ff" :weight bold :height 1.3))))
 '(highlight-indentation-current-column-face ((t (:background "#181522"))))
 '(highlight-indentation-face ((t (:background "#2e2942"))))
 '(hl-line ((t (:extend t :background "#30304e"))))
 '(info-menu-star ((t (:foreground "#d87373"))))
 '(info-node ((t (:foreground "#ffb733" :slant italic :weight bold))))
 '(info-title-1 ((t (:foreground "#938760" :weight bold))))
 '(info-title-2 ((t (:foreground "#bdb59b" :weight bold))))
 '(info-title-3 ((t (:foreground "#bdb59b" :weight bold))))
 '(info-title-4 ((t (:foreground "#e3cfcf" :weight bold))))
 '(isearch ((t (:background "lightslateblue" :foreground "black"))))
 '(isearch-group-1 ((t (:background "gainsboro" :foreground "black"))))
 '(isearch-group-2 ((t (:background "bisque" :foreground "black"))))
 '(line-number-current-line ((t (:inherit line-number :background "#231f32" :foreground "#ffb733"))))
 '(link ((t (:foreground "lightslateblue" :underline t))))
 '(link-visited ((t (:inherit link :foreground "orchid"))))
 '(lsp-headerline-breadcrumb-path-face ((t (:background "#8585ff" :foreground "#231f32"))))
 '(lsp-headerline-breadcrumb-separator-face ((t (:background "#8585ff" :foreground "#231f32" :height 0.8))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-doc-face :background "#8055cc" :foreground "#231f32" :weight bold))))
 '(magit-blame-hash ((t (:foreground "#656500"))))
 '(magit-blame-highlight ((t (:extend t :foreground "#2a662a"))))
 '(markdown-highlighting-face ((t (:background "salmon" :foreground "linen"))))
 '(match ((t (:background "#8584ff" :foreground "#00006b"))))
 '(minibuffer-prompt ((t (:background "#30304e" :foreground "lavender" :box (:line-width (1 . -1) :color "red" :style released-button) :weight bold))))
 '(mode-line ((t (:background "#181522" :foreground "#62629c" :box (:line-width (1 . -1) :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#545454" :foreground "#a8a8a8" :box (:line-width (1 . -1) :color "grey75") :weight light))))
 '(neo-file-link-face ((t (:inherit header-line))))
 '(neo-vc-conflict-face ((t (:foreground "#844a4a"))))
 '(neo-vc-default-face ((t (:foreground "#62629c"))))
 '(neo-vc-edited-face ((t (:foreground "#cc8400"))))
 '(neo-vc-unlocked-changes-face ((t (:foreground "#ff9d9d"))))
 '(neo-vc-up-to-date-face ((t (:foreground "#62629c"))))
 '(neo-vc-user-face ((t (:foreground "#e80000" :slant italic))))
 '(org-block ((t (:inherit shadow :extend t :background "#181522"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :extend t :background "#181522"))))
 '(org-code ((t (:inherit shadow :background "#181522"))))
 '(rainbow-delimiters-base-face ((t (:inherit default))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#a8a8a8"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ffe4b3"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#d1c0ba"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ffffb7"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#e3cfcf"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#c2f0c2"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ffd3d3"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#8585ff"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "#b7ffff"))))
 '(sml/col-number ((t (:foreground "moccasin"))))
 '(sml/filename ((t (:inherit sml/global :foreground "#a8ff51" :weight bold))))
 '(sml/git ((t (:foreground "#84b384"))))
 '(sml/line-number ((t (:foreground "moccasin" :inverse-video nil))))
 '(sml/modes ((t (:inherit sml/prefix :foreground "#8585ff"))))
 '(sml/modified ((t (:inherit sml/not-modified :foreground "#d87373" :weight bold))))
 '(sml/not-modified ((t (:foreground "#a8ff51"))))
 '(sml/outside-modified ((t (:inherit sml/not-modified :foreground "#820000"))))
 '(sml/prefix ((t (:inherit sml/global :foreground "#8585ff"))))
 '(sml/sudo ((t (:foreground "#7141c6"))))
 '(tab-bar ((t (:inherit variable-pitch :background "#080808" :foreground "#8584ff"))))
 '(tab-bar-tab-group-current ((t (:foreground "#8584ff" :box nil :weight bold))))
 '(tab-bar-tab-group-inactive ((t (:foreground "#303030"))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :foreground "#303030"))))
 '(tab-bar-tab-ungrouped ((t (:background "#303030" :foreground "#62629c"))))
 '(tab-line ((t (:inherit variable-pitch :background "#181522" :foreground "#62629c" :height 0.9))))
 '(tab-line-highlight ((t (:background "#820000" :foreground "#ffff9d" :box (:line-width (1 . 1) :style released-button)))))
 '(tab-line-tab-current ((t (:inherit mode-line-buffer-id))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :foreground "#62629c"))))
 '(tab-line-tab-modified ((t (:inherit w-mode-line-head-modified))))
 '(treemacs-git-commit-diff-face ((t (:inherit 'warning))))
 '(vc-state-base ((t (:foreground "slategray"))))
 '(vertical-border ((t (:inherit mode-line-inactive :background "#231f32" :foreground "#74564c"))))
 '(w-mode-line-head ((t (:foreground "#8181b0"))))
 '(w-mode-line-head-modified ((t (:foreground "#d87373"))))
 '(w-mode-line-tail ((t (:foreground "#8181b0"))))
 '(w-mode-line-tail-modified ((t (:foreground "#d87373"))))
 '(warning ((t (:foreground "gold" :weight bold))))
 '(widget-field ((t (:extend t :background "#b7b7ff" :foreground "#181522"))))
 '(woman-bold ((t (:inherit font-lock-type-face))))
 '(woman-italic ((t (:inherit font-lock-keyword-face)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(column-number-mode t)
 '(command-log-mode-is-global t)
 '(company-show-quick-access t)
 '(company-tooltip-align-annotations t)
 '(custom-enabled-themes '(smart-mode-line-dark))
 '(custom-safe-themes
   '("abd2ad651d2d0feb3aa165536cff555308d17068bc9c73f020a9e7faadf0720b" "a687c49ab637fb934e2676c782a891de0f2f0a0599e34b18471fcab9d27c1119" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "1084e940e1529866da525e07800656de811e23a569962506ffb00f007699386d" "05bf0101e1cc26c47c94fffc7275886a12c2b7fd5b47286672897e9f5ddcc4b2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(debug-on-error '(nil))
 '(delimit-columns-before "")
 '(desktop-save-mode t)
 '(display-fill-column-indicator t)
 '(display-fill-column-indicator-column t)
 '(display-line-numbers-type 'relative)
 '(doom-modeline-highlight-modified-buffer-name t)
 '(doom-modeline-mode t)
 '(elpy-rpc-ignored-buffer-size 204800)
 '(fill-column 79)
 '(flycheck-mode-line-prefix "  ")
 '(gc-cons-threshold 8000)
 '(git-gutter:added-sign "▎")
 '(git-gutter:deleted-sign "▎")
 '(git-gutter:modified-sign "▎")
 '(global-auto-revert-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(global-display-line-numbers-mode t)
 '(global-git-gutter-mode t)
 '(global-hl-line-mode t)
 '(global-tab-line-mode t)
 '(grep-command "grep --color=auto -nH --null -e ")
 '(grep-find-command
   '("find . -type f -exec grep --color=auto -nH --null -e  \\{\\} +" . 54))
 '(helm-minibuffer-history-mode t)
 '(helm-mode t)
 '(highlight-indentation-blank-lines t)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(lsp-completion-show-detail nil)
 '(lsp-java-autobuild-enabled nil)
 '(lsp-java-code-generation-generate-comments t)
 '(lsp-java-code-generation-hash-code-equals-use-instanceof t)
 '(lsp-java-code-generation-to-string-code-style "STRING_FORMAT")
 '(lsp-java-completion-import-order ["java" "javax" "org" "com" "se"])
 '(lsp-java-completion-max-results 10)
 '(lsp-java-configuration-maven-user-settings "/home/egugwen/.m2/settings.xml")
 '(lsp-java-configuration-runtimes [])
 '(lsp-java-configuration-update-build-configuration "interactive")
 '(lsp-java-content-provider-preferred nil)
 '(lsp-java-dependency-package-representation "hierarchical")
 '(lsp-java-format-settings-profile "JCAT code formatter")
 '(lsp-java-format-settings-url
   "/repo/egugwen/dj/jcat-common-code-formatter/src/main/resources/jcat-code-formatter.xml")
 '(lsp-java-import-gradle-enabled nil)
 '(lsp-java-max-concurrent-builds 2)
 '(lsp-java-save-actions-organize-imports t)
 '(lsp-java-server-launch-mode "LightWeight")
 '(lsp-java-theme "vscode")
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))
 '(lsp-keep-workspace-alive nil)
 '(lsp-lens-enable nil)
 '(lsp-log-max 100)
 '(lsp-treemacs-sync-mode t)
 '(max-lisp-eval-depth 9999)
 '(max-specpdl-size 10)
 '(menu-bar-mode nil)
 '(mode-line-compact 'long)
 '(neo-smart-open t)
 '(neo-vc-integration '(face char))
 '(nxml-child-indent 4)
 '(scroll-bar-mode nil)
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(sml/full-mode-string "  ")
 '(sml/mode-width 'right)
 '(sml/modified-char "  ")
 '(sml/name-width 50)
 '(sml/not-modified-char "  ")
 '(sml/outside-modified-char "  ")
 '(sml/pre-id-separator " ")
 '(sml/pre-modes-separator " ")
 '(sml/prefix-face-list
   '((":SU:" sml/sudo)
	 ("" sml/git)
	 (sml/projectile-replacement-format sml/projectile)
	 ("" sml/prefix)))
 '(sml/read-only-char "  ")
 '(sml/replacer-regexp-list
   '(("^~/org/" ":Org:")
	 ("^~/\\.emacs\\.d/elpa/" ":ELPA:")
	 ("^~/\\.emacs\\.d/" ":ED:")
	 ("^/sudo:.*:" ":SU:")
	 ("^~/Documents/" ":Doc:")
	 ("^~/Dropbox/" ":DB:")
	 ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
	 ("^~/[Gg]it/" ":Git:")
	 ("^~/[Gg]it[Hh]ub/" ":Git:")
	 ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")))
 '(sml/shorten-directory t)
 '(sml/shorten-mode-string "  ")
 '(sml/shorten-modes nil)
 '(sml/show-remote nil)
 '(sml/vc-mode-show-backend t)
 '(tab-bar-close-button-show nil)
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right))
 '(tab-bar-history-mode t)
 '(tab-bar-mode t)
 '(tab-bar-new-tab-choice "~/.emacs.d/init.el")
 '(tab-bar-select-tab-modifiers '(meta))
 '(tab-bar-show 1)
 '(tab-bar-tab-hints t)
 '(tab-line-close-button-show nil)
 '(tab-line-new-button-show nil)
 '(tab-line-switch-cycling t)
 '(tab-line-tab-name-function 'tab-line-tab-name-buffer)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(treemacs--fringe-indicator-bitmap 'treemacs--fringe-indicator-bitmap-default)
 '(treemacs-collapse-dirs 3)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-after-init t)
 '(treemacs-follow-mode t)
 '(treemacs-fringe-indicator-mode nil)
 '(treemacs-git-mode t)
 '(treemacs-project-follow-cleanup t)
 '(treemacs-recenter-after-file-follow 'on-distance)
 '(treemacs-recenter-after-tag-follow 'on-distance)
 '(treemacs-select-when-already-in-treemacs 'next-or-back)
 '(treemacs-workspace-switch-cleanup 'all)
 '(undo-no-redo t)
 '(warning-minimum-level :emergency)
 '(yas-global-mode t))
;;///////////////////////////////////////////////////////////////////////////////
;; Customized Theme
;;///////////////////////////////////////////////////////////////////////////////
(load-theme 'hacker-2023)
;;///////////////////////////////////////////////////////////////////////////////
;; Customized Mode Line
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
;;///////////////////////////////////////////////////////////////////////////////
(defun install-mode-line-pkgs()
  (use-package doom-modeline
	:straight t)
  (use-package smart-mode-line
	:straight t)
  (use-package mood-line
	:straight t)
;;  (defadvice vc-mode-line (after me/vc-mode-line () activate)
;;	"Strip backend from the VC information."
;;	(when (stringp vc-mode)
;;	  (let ((vc-text (replace-regexp-in-string "^ Git." "  " vc-mode)))
;;		(setq vc-mode vc-text))))
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
					   ;;			   (vc-mode vc-mode)
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
(w-mode-line)
;;///////////////////////////////////////////////////////////////////////////////
;; Set Global Keys
;;///////////////////////////////////////////////////////////////////////////////
(defun set-global-keys()
  "To set global keys."
  (global-set-key (kbd "C-c a") 'lsp-execute-code-action)
  (global-set-key (kbd "C-c b") 'magit-blame)
  (global-set-key (kbd "C-c c") 'customize)
  (global-set-key (kbd "C-c e") 'lsp-treemacs-errors-list)
  (global-set-key (kbd "C-c E") 'flycheck-list-errors)
  (global-set-key (kbd "C-c f") 'lsp-format-buffer)
  (global-set-key (kbd "C-c F") 'lsp-java-organize-imports)
  (global-set-key (kbd "C-c g") 'emx/garbage-collect)
  (global-set-key (kbd "C-c h") 'windmove-left)
  (global-set-key (kbd "C-c j") 'windmove-right)
  (global-set-key (kbd "C-c q") 'magit-blame-quit)
  (global-set-key (kbd "C-c r") 'tab-bar-close-tab)
  (global-set-key (kbd "C-c n") 'neotree-toggle)
  (global-set-key (kbd "C-c s") 'lsp-describe-session)
  (global-set-key (kbd "C-c t") 'treemacs)
  (global-set-key (kbd "C-c u") 'untabify)
  (global-set-key (kbd "C-c k") 'kill-line)
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)
  (global-set-key (kbd "C-c z") 'undo-redo)
  (global-set-key (kbd "C-c (") 'indent-region)
  (global-set-key (kbd "C-c =") 'maximize-window)
  (global-set-key (kbd "C-c .") 'xref-find-references)
  (global-set-key (kbd "C-c <up>") 'windmove-up)
  (global-set-key (kbd "C-c <down>") 'windmove-down)

  (global-set-key (kbd "M-n") 'tab-bar-new-tab)
  (global-set-key (kbd "M-o") 'lsp-ui-imenu)
  (global-set-key (kbd "M-RET") 'tab-next)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; rebind global keys to emacs extensions keys
  (global-set-key (kbd "M-x") 'helm-M-x)
  )
(set-global-keys)
;;///////////////////////////////////////////////////////////////////////////////
;; Set Global Switch
;;///////////////////////////////////////////////////////////////////////////////
;; global switch
(defun global-switch()
  "Set global switch."
  (turn-on-auto-revert-mode)
  (winner-mode)
  (global-tab-line-mode t)
  (highlight-indentation-mode)
  (setq tab-bar-separator "  "
		tab-line-separator "  "
		read-process-output-max 2000
		tab-bar-button-margin '(5 . 2)
		tags-file-name "~/.emacs.d/TAGS"
		mode-line-end-spaces nil)
  (set-display-table-slot standard-display-table
						  'vertical-border
						  (make-glyph-code ?│)))
(global-switch)
;;///////////////////////////////////////////////////////////////////////////////
;; Hooks for programming language modes and other modes
;;///////////////////////////////////////////////////////////////////////////////
(defvar prog-modes "shell-script java python")
(defvar non-prog-modes "org term shell eshell treemacs neotree")
(defun prog-env-hook()
  "Programming environment features."
  (auto-fill-mode)
  (global-display-fill-column-indicator-mode 1)
  )
(defun non-prog-env-hook()
  "Non-programming environment features."
  (display-line-numbers-mode t))

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
  (add-hook 'org-mode (lambda () (setq fill-column -1)))
  (add-hook 'emacs-lisp-mode-hook (lambda() (setq fill-column -1)))
)
(non-common-hooks)

(provide 'init)
;;; init.el ends here
