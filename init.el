;;; package --- Summary"
;;; Commentary:
;; load Spacemacs's initialization file, "~" is equivalent to "$HOME"
;; (load-file "/repo/egugwen/ws/.spacemacs.d/init.el")
;; ///////////////////////////////////////////////////////////////////////////////
;; PART1: Basic Configurations
;; ///////////////////////////////////////////////////////////////////////////////
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;; Code:
(package-initialize)

;; basic options configuration
;; disable defatul startup messages, show scratch file instead
(setq inhibit-startup-message t)
(setq visible-bell t)

;; disable scroll-bar, tool-bar, tooltip, and menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; auto-reload change from disk
(global-auto-revert-mode t)
(turn-on-auto-revert-mode)

;; enable winner-mode
(winner-mode)

;; display fill column indicator and dynamically change fill-column value with
;; different modes
(setq-default fill-column 79)
(global-display-fill-column-indicator-mode 1)
(auto-fill-mode t)
(add-hook 'java-mode-hook
		  (lambda()
			(progn
			  (setq fill-column 120)
			  (setq tab-width 4)
			  (setq indent-tabs-mode nil))
			)
		  )
(add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
(add-hook 'org-mode (lambda () (setq fill-column -1)))
(add-hook 'emacs-lisp-mode-hook (lambda() (setq fill-column -1)))
;; automatically switch to org-mode for .org files
;;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; customize highlight in man pages
(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

(set-fringe-mode '(100 . 0))
;; make the left fringe 4 pixels wide and the right disappear
;;(fringe-mode '(40 . 0))

;; show cursorline
(global-hl-line-mode 1)

;;; -- show line number ---
(column-number-mode)
(global-display-line-numbers-mode 1)
(set-face-attribute 'line-number-current-line nil :background "#231f32" :foreground "#ffb733")
;; disable line number in some modes
(dolist (mode '(org-mode-hook
				 term-mode-hook
				 shell-mode-hook
				 treemacs-mode-hook
				 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; highlight matches parentheses
(show-paren-mode 1)

;; save session
(desktop-save-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-template-field ((t (:background "#b7b7ff" :foreground "#181522"))))
 '(company-tooltip ((t (:background "#30304e"))))
 '(company-tooltip-annotation ((t (:foreground "#e3cfcf"))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :foreground "#c2f0c2"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-common :foreground "#ff9d9d"))))
 '(company-tooltip-selection ((t (:extend t :background "#181522"))))
 '(custom-comment ((t (:background "#181522" :foreground "#d1ccba"))))
 '(fill-column-indicator ((t (:inherit font-lock-comment-face))))
 '(git-gutter:deleted ((t (:inherit default :foreground "#e80000" :weight bold))))
 '(header-line ((t (:background "#8585ff" :foreground "#231f32" :inverse-video t :box (:line-width (1 . -1) :color "red" :style released-button)))))
 '(helm-selection ((t (:inherit hl-line))))
 '(helm-source-header ((t (:extend t :background "#181522" :foreground "#b7b7ff" :weight bold :height 1.3))))
 '(info-menu-star ((t (:foreground "#d87373"))))
 '(info-node ((t (:foreground "#ffb733" :slant italic :weight bold))))
 '(info-title-1 ((t (:foreground "#938760" :weight bold))))
 '(info-title-2 ((t (:foreground "#bdb59b" :weight bold))))
 '(info-title-3 ((t (:foreground "#bdb59b" :weight bold))))
 '(info-title-4 ((t (:foreground "#e3cfcf" :weight bold))))
 '(isearch ((t (:background "lightslateblue" :foreground "black"))))
 '(isearch-group-1 ((t (:background "gainsboro" :foreground "black"))))
 '(isearch-group-2 ((t (:background "bisque" :foreground "black"))))
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
 '(sml/git ((t (:foreground "#84b384"))))
 '(sml/line-number ((t (:foreground "moccasin" :inverse-video nil))))
 '(sml/modes ((t (:inherit sml/prefix :foreground "#8585ff"))))
 '(sml/modified ((t (:inherit sml/not-modified :foreground "#d87373" :weight bold))))
 '(sml/not-modified ((t (:foreground "#a8ff51"))))
 '(sml/outside-modified ((t (:inherit sml/not-modified :foreground "#820000"))))
 '(sml/prefix ((t (:inherit sml/global :foreground "#8585ff"))))
 '(sml/sudo ((t (:foreground "#7141c6"))))
 '(treemacs-git-commit-diff-face ((t (:inherit 'warning))))
 '(vc-state-base ((t (:foreground "slategray"))))
 '(warning ((t (:foreground "tomato" :weight bold))))
 '(widget-field ((t (:extend t :background "#b7b7ff" :foreground "#181522"))))
 '(woman-bold ((t (:inherit font-lock-type-face))))
 '(woman-italic ((t (:inherit font-lock-keyword-face)))))

;; tab-bar customization
;;(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
	  tab-bar-show 1
	  tab-bar-tab-hints t
	  tab-bar-new-tab-choice "~/.emacs.d/init.el"
	  tab-bar-separator ""
	  tab-bar-button-margin '(5 . 2)
	  )
'(tab-bar ((t (:inherit mode-line))))

;; tab line mode customization
;; (global-tab-line-mode t)
;; (setq tab-line-new-button-show nil)  ;; do not show add-new button
;; (setq tab-line-close-button-show nil)  ;; do not show close button
;; (setq tab-line-separator " > ")
;; ;; tab color settings
;; (set-face-attribute 'tab-line nil ;; background behind tabs
;;       :background "gray40"
;;       :foreground "gray60" :distant-foreground "gray50"
;;       :height 1.0 :box nil)
;; (set-face-attribute 'tab-line-tab nil ;; active tab in another window
;;       :inherit 'tab-line
;;       :foreground "gray70" :background "gray90" :box nil)
;; (set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
;;       :background "#b34cb3" :foreground "white" :box nil)
;; (set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
;;       :background "gray60" :foreground "black" :box nil)
;; (set-face-attribute 'tab-line-highlight nil ;; mouseover
;;       :background "white" :foreground 'unspecified)

;; -- ctags --
(setq tags-file-name "~/.emacs.d/TAGS")

;; Set frame transparency
;;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))



;; remove dashes in mode line
(setq-default mode-line-end-spaces nil)

;; self-customized mode line
;; 1) size
;;(set-face-attribute 'mode-line nil
;;                    :background "#353644"
;;                    :foreground "white"
;;                    :box '(:line-width 8 :color "#353644")
;;                    :overline nil
;;                    :underline nil)
;;
;;(set-face-attribute 'mode-line-inactive nil
;;                    :background "#565063"
;;                    :foreground "white"
;;                    :box '(:line-width 8 :color "#565063")
;;                    :overline nil
;;                    :underline nil)
;;;; 2) git branch
;;(vc-mode)
;;;; 3) right-justified text
;;'(:eval (propertize
;;         " " 'display
;;         `((space :align-to (- (+ right right-fringe right-margin)
;;                               ,(+ 3 (string-width mode-name)))))))

;;///////////////////////////////////////////////////////////////////////////////
;; PART2: Package Management
;;///////////////////////////////////////////////////////////////////////////////
;; use-package
(eval-when-compile
  (add-to-list 'load-path "/repo/egugwen/github/use-package")
  (require 'use-package))
(setq use-package-always-ensure t)

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
;; causeing problem: Debugger entered--Lisp error: (file-missing "/home/egugwen/.emacs.d/straight/build/el-patch/el-patch.el"
;;(straight-use-package 'el-patch)

;;///////////////////////////////////////////////////////////////////////////////
;; PART3: Package Installation and Configuration
;;///////////////////////////////////////////////////////////////////////////////
;; themes
;; doom-themes
(use-package doom-themes
			 :straight t)
;; native-compiler-error
;;(use-package doom-modeline
;;			 :straight t
;;			 :init (doom-modeline-mode 1)
;;			 )
(use-package multiple-cursors
			 :straight t)

;; helm: http://tuhdo.github.io/helm-intro.html
;; also has compiling error
(use-package helm
  :straight t
  :init
  (helm-mode 1))

;; git
;; https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter :straight t
  :init (git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))
(use-package git-gutter-fringe :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;;evil
(use-package evil
			 :straight t)

;; -- python development environment
;; error in process sentinel: elpy-rpc--default-error-callback: peculiar error: "exited abnormally with code 1"
;;(use-package elpy
;;  :straight t)
(use-package flycheck
			 :straight t
			 :ensure t
			 :init (global-flycheck-mode) (flycheck-display-error-at-point))

;; fatal: unable to access 'https://codeberg.org/ideasman42/emacs-py-autopep8.git/
;;(use-package py-autopep8
;;  :straight t)
(use-package blacken
			 :straight t)
;; due to libgccjit.so: error: error invoking gcc driver, cannot work
;;(use-package ein
;;  :straight t)
;;(use-package pytest
;;  :straight t)
;; error in process sentinel: elpy-rpc--default-error-callback: peculiar error: "exited abnormally with code 1"
;;(elpy-enable)
;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;(setq python-shell-interpreter "jupyter"
;;      python-shell-interpreter-args "console --simple-prompt"
;;      python-shell-prompt-detect-failure-warning nil)
;;(add-to-liOA 'python-shell-completion-native-disabled-interpreters
;;             "jupyter")

(when (string-match "28." (emacs-version))
  ;; based on json-mode, add function to beautify-json file content and bind to key C-c C-b
  (defun beautify-json ()
	(interactive)
	(let ((b (if mark-active (min (point) (mark)) (point-min)))
		  (e (if mark-active (max (point) (mark)) (point-max))))
	  (shell-command-on-region b e
							   "python -m json.tool" (current-buffer) t)))
  (use-package json-mode
			   :bind (("C-c C-b" . beautify-json))
			   )
  (use-package ivy
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
  (use-package lsp-ui :straight t)
  (use-package yasnippet :straight t))
(use-package all-the-icons
			 :straight t)
;; causing problems
;;(use-package dired-hacks
;;             :straight (el-patch :type git :host github :repo "Fuco1/dired-hacks"))

;; org mode
;;(use-package org-roam
;;  :straight (el-patch :type git :host github :repo "org-roam/org-roam"))
(defun emc/org-mode-setup()
  (variable-pitch-mode)
  (auto-fill-mode 0)
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
			 :hook (org-mode . org-bullets-mode)
			 :custom
			 (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(use-package visual-fill-column
;;             :straight (el-patch :type git :host github :repo "joostkremers/visual-fill-column")
			 :straight t
			 :hook (org-mode . emc/org-mode-visual-fill))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'java-mode-hook #'rainbow-delimiters-mode))

;; mode line customization (packages: solarized-theme + smart-mode-line)
;;(use-package solarized-theme
;;             :config
;;             (load-theme 'solarized-light t)
;;             (let ((line (face-attribute 'mode-line :underline)))
;;               (set-face-attribute 'mode-line          nil :overline   line)
;;               (set-face-attribute 'mode-line-inactive nil :overline   line)
;;               (set-face-attribute 'mode-line-inactive nil :underline  line)
;;               (set-face-attribute 'mode-line          nil :box        nil)
;;               (set-face-attribute 'mode-line-inactive nil :box        nil)
;;               (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))
;;
;;(use-package moody
;;             :config
;;             (setq x-underline-at-descent-line t)
;;             (moody-replace-mode-line-buffer-identification)
;;             (moody-replace-vc-mode)
;;             (moody-replace-eldoc-minibuffer-message-function)
;;		 (customize-set-variable 'moody-mode-line-height 18)
;;		 (set-face-attribute 'mode-line nil :box nil :foreground "#7e7486")
;;		 (set-face-attribute 'mode-line-inactive nil :box nil)
;;		 )

;; mode line customization (packages: smart-mode-line, smart-mode-line-powerline-theme)
(require 'smart-mode-line)
(require 'smart-mode-line-dark-theme)
;; powerline theme cannot work due to native compiling issue
;;(setq powerline-arrow-shape 'curve)
;;(setq powerline-default-separator-dir '(right . left))
(setq sml/theme 'dark)
;;(sml/setup)
(use-package mood-line :straight t)
(defadvice vc-mode-line (after me/vc-mode-line () activate)
		   "Strip backend from the VC information."
		   (when (stringp vc-mode)
			 (let ((vc-text (replace-regexp-in-string "^ Git." "  " vc-mode)))
			   (setq vc-mode vc-text))))

;; nicer vertical border
(set-face-attribute 'vertical-border nil :background "#231f32" :foreground "#74564c")
(set-display-table-slot standard-display-table
						'vertical-border
						(make-glyph-code ?│))


;; --- which-key ---
(use-package which-key
  :straight t
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; set wonderful global keys
(global-set-key (kbd "C-c b") 'magit-blame)
(global-set-key (kbd "C-c c") 'customize)
(global-set-key (kbd "C-c e") 'lsp-treemacs-errors-list)
(global-set-key (kbd "C-c E") 'flycheck-list-errors)
(global-set-key (kbd "C-c f") 'lsp-format-buffer)
(global-set-key (kbd "C-c F") 'lsp-java-organize-imports)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-right)
(global-set-key (kbd "C-c q") 'magit-blame-quit)
(global-set-key (kbd "C-c l") 'tab-switcher)
(global-set-key (kbd "C-c n") 'neotree-toggle)
(global-set-key (kbd "C-c t") 'treemacs)
(global-set-key (kbd "C-c u") 'untabify)
(global-set-key (kbd "C-c k") 'kill-line)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c (") 'indent-region)
(global-set-key (kbd "C-c =") 'maximize-window)
(global-set-key (kbd "C-c .") 'xref-find-references)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; rebind global keys to emacs extensions keys
(global-set-key (kbd "M-x") 'helm-M-x)


;; Java Development Environment
;; Install Failure
;;Use-Package  Meghanada
;; :straight t)
;;add-hook 'java-mode-hook
;;         (lambda ()
;;           ;; meghanada-mode on
;;           (meghanada-mode t)
;;           (flycheck-mode +1)
;;           (setq c-basic-offset 2)
;;           ;; use code format
;;           (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
;;cond
;;  ((eq system-type 'windows-nt)
;;   (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
;;   (setq meghanada-maven-path "mvn.cmd"))
;;  (t
;;   (setq meghanada-java-path "java")
;;   (setq meghanada-maven-path "mvn")))

;; compiling error
;; Debugger entered--Lisp error: (error "Lisp nesting exceeds `max-lisp-eval-depth'")

;;
;; auto-completion
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package command-log-mode)
(add-hook 'LaTeX-mode-hook 'command-log-mode)

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))
;;
;; tree-explorer
;;
(use-package treemacs
  :straight t
  :config
  (treemacs-git-mode 'deferred)
  )

(use-package anzu
			 :straight t)

(use-package all-the-icons-dired
			 :straight t)
(load "all-the-icons-dired.el")
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package treemacs-icons-dired
			 :straight t)

(use-package neotree
			 :straight t)

(use-package term
			 :commands term
			 :config
			 (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
			 ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

			 ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
			 (setq term-prompt-regexp ""))


;;
;; mode-line-format customization (start)
;; functions
;;
;;(defun mood-line--format (left right)
;;  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
;;  (let ((reserve (length right)))
;;	(concat left
;;			" "
;;			(propertize " "
;;						'display `((space :align-to (- right ,reserve))))
;;			right)))
;;
;;(defun mood-line-segment-position ()
;;  "Displays the current cursor position in the mode-line."
;;  (concat "%l:%c"
;;          (when mood-line-show-cursor-point (propertize (format ":%d" (point)) 'face 'mood-line-unimportant))
;;          (propertize " %p%%  " 'face 'mood-line-unimportant)))

;; copy of mood-line (start)
;; copy of mood-line (end)

;;reset mode-line-format
;;(setq-default mode-line-format
;;			  '((:eval
;;				 (mood-line--format
;;				  ;; left segment
;;				  (format-mode-line
;;				   '(" "
;;					 (:eval (mood-line-segment-cursor-position))
;;					 (:eval (mood-line-segment-buffer-status))
;;					 (:eval (mood-line-segment-buffer-name))
;;					 (:eval (mood-line-segment-anzu))))
;;				  ;; right segment
;;				  (format-mode-line
;;					 (:eval (mood-line-segment-eol))
;;					 (:eval (mood-line-segment-encoding))
;;					 (:eval (mood-line-segment-vc))
;;					 (:eval (mood-line-segment-major-mod))
;;					 (:eval (mood-line-segment-misc-info))
;;					 (:eval (mood-line-segment-checker))
;;					 (:eval (mood-line-segment-process))
;;					 " ")))))

;; investigate fonts
;;(set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
;;(set-face-attribute 'default nil :font "SourceCodePro")
;;(set-frame-font "Sourcecodepro" nil t)
;;(face-remap-add-relative 'default :family "Arial" :height 140)
;;(set-face-attribute 'default (selected-frame) :height 50)
;;(all-the-icons-insert-icons-for 'alltheicon)
;;(find-font (font-spec :name "Source Code Pro"))
;;(set-face-font 'default "fontset-standard")

;;
;; mode-line-format customization (end)
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(command-log-mode-is-global t)
 '(company-show-quick-access t)
 '(custom-enabled-themes '(smart-mode-line-dark Hacker))
 '(custom-safe-themes
   '("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "1084e940e1529866da525e07800656de811e23a569962506ffb00f007699386d" "05bf0101e1cc26c47c94fffc7275886a12c2b7fd5b47286672897e9f5ddcc4b2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(delimit-columns-before "")
 '(display-fill-column-indicator t)
 '(display-fill-column-indicator-column t)
 '(flycheck-mode-line-prefix "  ")
 '(git-gutter:added-sign "▎")
 '(git-gutter:deleted-sign "▎")
 '(git-gutter:modified-sign "▎")
 '(global-display-fill-column-indicator-mode t)
 '(global-git-gutter-mode t)
 '(grep-command "grep --color=auto -nH --null -e ")
 '(grep-find-command
   '("find . -type f -exec grep --color=auto -nH --null -e  \\{\\} +" . 54))
 '(kill-whole-line t)
 '(lsp-java-completion-import-order ["java" "javax" "org" "com" "se"])
 '(lsp-java-configuration-maven-user-settings "/home/egugwen/.m2/settings.xml")
 '(lsp-java-configuration-runtimes [])
 '(lsp-java-format-settings-profile "JCAT code formatter")
 '(lsp-java-format-settings-url
   "/repo/egugwen/dj/jcat-common-code-formatter/src/main/resources/jcat-code-formatter.xml")
 '(lsp-java-import-gradle-enabled nil)
 '(lsp-java-save-actions-organize-imports t)
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))
 '(lsp-lens-enable nil)
 '(mode-line-compact 'long)
;; '(mode-line-format
;;   '(" " " %l:%C(%p)"
;;	 (vc-mode vc-mode)
;;	 mode-line-modified mode-line-buffer-identification sml/pre-id-separator "(%I)" mode-line-buffer-identification))
 '(neo-smart-open t)
 '(nxml-child-indent 4)
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
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
 '(tab-width 4)
 '(undo-no-redo t)
 '(warning-minimum-level :emergency))

;; mine
;;
;;(" " " %l:%C(%p)"
;; (vc-mode vc-mode)
;; mode-line-modified mode-line-buffer-identification sml/pre-id-separator "(%I)" flycheck-error-list-mode)

;; default
;;(("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;  (vc-mode vc-mode)
;;  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; doom-modeline
;; ("%e" (:eval (doom-modeline-format--main)))

;;///////////////////////////////////////////////////////////////////////////////
;; PART4: Performance Tuning
;;///////////////////////////////////////////////////////////////////////////////
(defun ap/garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
					for used = (* used size)
					for free = (* (or free 0) size)
					for total = (file-size-human-readable (+ used free))
					for used = (file-size-human-readable used)
					for free = (file-size-human-readable free)
					concat (format "%s: %s + %s = %s\n" type used free total))))

;;///////////////////////////////////////////////////////////////////////////////
;; PART5: Customized Mode Line
;;///////////////////////////////////////////////////////////////////////////////
(defgroup w-mode-line nil
  "I want to have a wonderful mode-line: w-mode-line.

  The w-mode-line consists of several segments:
  1. left part: head label - left part segments
  2. right part: right part segments - tail label.
  The segments are separated by separator label."
  :group 'mode-line)

(defgroup w-mode-line-faces nil
  "Yes, it includes all w-mode-line faces."
  :group 'w-mode-line
  :group 'faces)

(defgroup w-mode-line-labels '()
  "Group for mode-line labels."
  :group 'w-mode-line)

(defface w-mode-line-head
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

(defcustom w-mode-line-head-label "  "
  "String being used as head label of w-mode-line."
  :type 'string
  :group 'w-mode-line-labels)

(defcustom w-mode-line-tail-label "  "
  "String being used as head label of w-mode-line."
  :type 'string
  :group 'w-mode-line-labels)

(defun w-mode-line-seg-head()
  "To return head segment of w-mode-line."
  (propertize
   w-mode-line-head-label
   'face 'w-mode-line-head))

(w-mode-line-seg-head)

(defun w-mode-line-seg-tail()
  "To return tail segment of w-mode-line."
  (propertize
   w-mode-line-tail-label
   'face 'w-mode-line-tail))

(w-mode-line-seg-tail)

(defun w-mode-line-seg-encoding()
  "To return encoding segment."
  (let*
	((sys (coding-system-plist buffer-file-coding-system))
	 (sym (plist-get sys :name)))
  (propertize
   (upcase (symbol-name sym))
   'face 'font-lock-string-face)))

(w-mode-line-seg-encoding)

(defun w-mode-line()
  "Enable w-mode-line."
  (setq mode-line-format
		'((:eval
		   (mood-line--format
			(format-mode-line
			 '(" "
			   (:eval (w-mode-line-seg-head))
			   mode-line-modified
			   (vc-mode vc-mode)
			   mode-line-buffer-identification)
			 )

			(format-mode-line
			 '((:eval (w-mode-line-seg-encoding))
			   (:eval (w-mode-line-seg-tail))
			   " ")))))))

(w-mode-line)

;;///////////////////////////////////////////////////////////////////////////////
;; Ending
;;///////////////////////////////////////////////////////////////////////////////
(provide 'init)
;;; init.el ends here

