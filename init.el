
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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

;; disable warning
(setq warning-minimum-level :emergency)

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
			 (setq fill-column 120))
			 (setq tab-width 4)))
(add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
(add-hook 'org-mode (lambda () (setq fill-column -1)))
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'java-mode-hook #'rainbow-delimiters-mode)

;; customize highlight in man pages
(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

(set-fringe-mode 10)
;; make the left fringe 4 pixels wide and the right disappear
;;(fringe-mode '(40 . 0))

;; show cursorline
(global-hl-line-mode 1)

;; show line number
(column-number-mode)
(global-display-line-numbers-mode 1)
;; relative number
(setq display-line-numbers-type 'relative)
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
 '(fill-column-indicator ((t (:inherit font-lock-comment-face))))
 '(isearch ((t (:background "lightslateblue" :foreground "black"))))
 '(isearch-group-1 ((t (:background "gainsboro" :foreground "black"))))
 '(isearch-group-2 ((t (:background "bisque" :foreground "black"))))
 '(sml/col-number ((t (:foreground "moccasin"))))
 '(sml/git ((t (:foreground "#84b384"))))
 '(sml/line-number ((t (:foreground "moccasin" :inverse-video nil))))
 '(sml/modes ((t (:inherit sml/prefix :foreground "#8585ff"))))
 '(sml/modified ((t (:inherit sml/not-modified :foreground "#d87373" :weight bold))))
 '(sml/not-modified ((t (:foreground "#a8ff51"))))
 '(sml/outside-modified ((t (:inherit sml/not-modified :foreground "#820000"))))
 '(sml/prefix ((t (:inherit sml/global :foreground "#8585ff"))))
 '(sml/sudo ((t (:foreground "#7141c6"))))
 '(vc-state-base ((t (:foreground "slategray"))))
 '(woman-bold ((t (:inherit font-lock-type-face))))
 '(woman-italic ((t (:inherit font-lock-keyword-face)))))

;; tab-bar customization
;;(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
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
;; (setq tab-line-separator "")  ;; set it to empty
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

;; helm: http://tuhdo.github.io/helm-intro.html
;; also has compiling error
;;(straight-use-package 'helm)
;;(helm-mode 1)

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

;; doom-themes
;;(use-package doom-themes
;;  :straight t)
;; doom-themes
;; native-compiler-error
;;(use-package doom-modeline
;; :straight t
;; :ensure t
;; :init (doom-modeline-mode 1)
;; )

;;evil
;;(use-package evil
;;  :straight t)

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
;;(add-to-list 'python-shell-completion-native-disabled-interpreters
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
  (use-package lsp-mode
    :straight t
    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-c l")
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	   (XXX-mode . lsp)
	   ;; if you want which-key integration
	   (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)
  )
(use-package all-the-icons
  :straight t)
(use-package dired-hacks
	:straight (el-patch :type git :host github :repo "Fuco1/dired-hacks"))
;; which-key
;;(use-package emacs-which-key
;;  :straight t)

;; org mode
;;(use-package org-roam
;;  :straight (el-patch :type git :host github :repo "org-roam/org-roam"))
(defun emc/org-mode-setup()
  (org-indent-mode)
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
  :hook (org-mode . emc/org-mode-setup))
(setq org-ellipsis " ▾"
	  org-agenda-files
	  '("/repo/egugwen/dj/redwood/resources/worknote/wnote.org")
	  org-agenda-start-with-log-mode t
	  org-log-done 'time
	  org-log-into-drawer t
	  org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
		(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(straight-use-package 'el-patch)
(use-package visual-fill-column
  :straight (el-patch :type git :host github :repo "joostkremers/visual-fill-column")
  :hook (org-mode . emc/org-mode-visual-fill))

(use-package rainbow-delimiters
	:straight t)

;; Symbol's value as variable is void: git
;;(straight-use-package
;; 'emacs-which-key :type git :host github :repo "justbur/emacs-which-key")

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
;;	     (customize-set-variable 'moody-mode-line-height 18)
;;	     (set-face-attribute 'mode-line nil :box nil :foreground "#7e7486")
;;	     (set-face-attribute 'mode-line-inactive nil :box nil)
;;	     )

;; mode line customization (packages: smart-mode-line, smart-mode-line-powerline-theme)
(require 'smart-mode-line)
(require 'smart-mode-line-dark-theme)
;; powerline theme cannot work due to native compiling issue
;;(setq powerline-arrow-shape 'curve)
;;(setq powerline-default-separator-dir '(right . left))
(setq sml/theme 'dark)
(sml/setup)

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


(eval-when-compile
  (add-to-list 'load-path "/repo/egugwen/github/use-package")
  (require 'use-package))
(setq use-package-always-ensure t)

;; --- which-key ---
;;(eval-when-compile
;;  (add-to-list 'load-path "/repo/egugwen/github/emacs-which-key")
;;  (require 'which-key))
;;(which-key-setup-side-window-right-bottom)
;;(which-key-mode)
(use-package which-key
  :straight t)
(which-key-setup-side-window-right-bottom)
(which-key-mode)

;; set global keys
(global-set-key (kbd "C-d") 'kill-line)
(global-set-key (kbd "C-z") 'maximize-window)
(global-set-key (kbd "C-M-.") 'xref-find-references)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; java development environment
;; install failure
;;use-package  meghanada
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
;;use-package lsp-java
;; :straight t
;; :config (add-hook 'java-mode-hook 'lsp))

;; auto-completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package command-log-mode)
(add-hook 'LaTeX-mode-hook 'command-log-mode)

;; tree-explorer
(use-package treemacs
  :straight t)
(treemacs-git-mode 'deferred)

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
;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-theme 'nerd)

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp ""))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(company-show-quick-access t)
 '(custom-enabled-themes '(smart-mode-line-dark Hacker))
 '(custom-safe-themes
   '("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "1084e940e1529866da525e07800656de811e23a569962506ffb00f007699386d" "05bf0101e1cc26c47c94fffc7275886a12c2b7fd5b47286672897e9f5ddcc4b2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(delimit-columns-before "")
 '(display-fill-column-indicator t)
 '(display-fill-column-indicator-column t)
 '(flycheck-mode-line-prefix "  ")
 '(global-display-fill-column-indicator-mode t)
 '(kill-whole-line t)
 '(mode-line-compact 'long)
 '(mode-line-format
   '(" " " %l:%C(%p)"
	 (vc-mode vc-mode)
	 mode-line-modified mode-line-buffer-identification sml/pre-id-separator "(%I)"))
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
 '(tab-width 4)
 '(undo-no-redo t)
 '(warning-minimum-level :emergency))
