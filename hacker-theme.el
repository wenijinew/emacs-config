;;; hacker-theme.el
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup hacker-theme nil
  "Options for the hacker theme."
  :group 'doom-themes)

(defcustom hacker-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'hacker-theme
  :type '(choice integer boolean))

(defcustom hacker-blue-modeline nil
  "If non-nil, mode-line's color will be blue instead of the default purple."
  :group 'hacker-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme hacker
  "A dark theme inspired by 1337 Theme."

  ;; name        default   256       16
  ((bg         '("#181522" "#181522" nil))
   (bg-alt     '("#231f32" "#231f32" nil))
   (base0      '("#002222" "#003b3b" "black"))
   (base1      '("#101010" "#101010" "brightblack"))
   (base2      '("#303030" "#626262" "brightblack"))
   (base3      '("#46342e" "#46342e" "brightblack"))
   (base4      '("#373700" "#373700" "brightblack"))
   (base5      '("#373224" "#373224" "brightblack"))
   (base6      '("#4e4e7d" "#4e4e7d" "brightblack"))
   (base7      '("#8181b0" "#8181b0" "brightblack"))
   (base8      '("#dfdfeb" "#dfdfeb" "white"))
   (fg         '("#b0b0ce" "#b0b0ce" "brightwhite"))
   (fg-alt     '("#b7b7ff" "#b7b7ff" "white"))

   (grey base7)
   (white        '("#FFFFFF" "#FFFFFF" "white"))
   (red          '("#820000" "#820000" "red"))
   (orange       '("#ffb733" "#ffb733" "brightred"))
   (green        '("#5ab400" "#5ab400" "green"))
   (light-green  '("#a8ff51" "#a8ff51" "green"))
   (teal         '("#47d247" "#47d247" "brightgreen"))
   (yellow       '("#ffff9d" "#ffff9d" "brightyellow"))
   (light-yellow '("#ffff4f" "#ffff4f" "brightyellow"))
   (blue         '("#8584ff" "#8584ff" "brightblue"))
   (dark-blue    '("#30304e" "#30304e" "blue"))
   (magenta      '("#4f2b8f" "#4f2b8f" "brightmagenta"))
   (violet       '("#8055cc" "#8055cc" "magenta"))
   (dark-violet  '("#4f2b8f" "#4f2b8f" "magenta"))
   (cyan         '("#00b7b7" "#00b7b7" "brightcyan"))
   (dark-cyan    '("#3d7474" "#3d7474" "cyan"))

   ;; component focused
   (bottomline-blue '("#181522" "#181522" "blue"))
   (vcmodified-blue '("#8181b0" "#8181b0" "blue"))
   (vcdeleted-red '("#d87373" "#d87373" "red"))

   ;; face categories -- required for all themes
   (highlight      white)
   (vertical-bar   base2)
   (selection      base5)
   (builtin        dark-blue)
   (comments       base6)
   (doc-comments   base6)
   (constants      orange)
   (functions      blue)
   (keywords       red)
   (methods        dark-blue)
   (operators      red)
   (type           yellow)
   (strings        light-yellow)
   (variables      yellow)
   (numbers        orange)
   (region         (doom-darken base5 0.5))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    vcmodified-blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (if hacker-blue-modeline base8 bottomline-blue))
   (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-fg     base8)
   (modeline-fg-alt blue)

   (-modeline-pad
    (when hacker-padded-modeline
      (if (integerp hacker-padded-modeline) hacker-padded-modeline 4))))

  ;;;; Base theme face overrides
  (((highlight &override) :foreground base8)
   (lazy-highlight :background base4 :foreground fg :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (tooltip :background base2 :foreground fg)

   ;;;; all-the-icons
   (all-the-icons-dblue    :foreground bottomline-blue)
   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)
   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background base6)
   (centaur-tabs-selected-modified
    :inherit 'centaur-tabs-selected :foreground fg :weight 'bold)
   (centaur-tabs-unselected-modified
    :inherit 'centaur-tabs-unselected :foreground fg :weight 'bold)
   (centaur-tabs-modified-marker-selected
    :inherit 'centaur-tabs-selected :foreground fg)
   (centaur-tabs-modified-marker-unselected
    :inherit 'centaur-tabs-unselected :foreground fg)
   ;;;; company
   (company-tooltip-selection     :background region)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; dashboard
   (dashboard-heading :foreground green :weight 'bold)
   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if hacker-blue-modeline base8 bottomline-blue))
   (doom-modeline-buffer-file :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   (doom-modeline-buffer-minor-mode :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-modified :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :inherit 'mode-line-emphasis)
   (doom-modeline-debug :inherit 'mode-line-emphasis)
   (doom-modeline-evil-insert-state :foreground cyan)
   (doom-modeline-evil-visual-state :foreground yellow)
   (doom-modeline-info :inherit 'mode-line-emphasis)
   (doom-modeline-lsp-success :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-persp-name :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-project-dir :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-project-parent-dir :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-urgent :inherit 'mode-line-emphasis)
   (doom-modeline-warning :inherit 'mode-line-emphasis)
   ;;;; doom-themes
   (doom-themes-treemacs-root-face :foreground fg :weight 'ultra-bold :height 1.2)
   (doom-themes-treemacs-file-face :foreground fg)
   ;;;; ivy
   (counsel-active-mode :foreground (doom-lighten base6 0.1))
   (ivy-current-match :background bg)
   (ivy-minibuffer-match-face-2 :foreground (doom-lighten base6 0.1) :weight 'extra-bold)
   ;;;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground cyan)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)
   ;;;; lsp-mode
   (lsp-lens-face              :foreground base7 :height 0.8)
   ;;;; org <built-in>
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   ;;;; org-pomodoro
   (org-pomodoro-mode-line :inherit 'mode-line-emphasis) ; unreadable otherwise
   (org-pomodoro-mode-line-overtime :inherit 'org-pomodoro-mode-line)
   (org-pomodoro-mode-line-break :inherit 'org-pomodoro-mode-line)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)
   ;;;; rjsx-mode
   (rjsx-tag :foreground blue)
   (rjsx-attr :foreground cyan :slant 'italic :weight 'medium)
   ;;;; treemacs
   (treemacs-root-face :foreground fg :weight 'ultra-bold :height 1.2)
   (treemacs-directory-face :foreground fg)
   (treemacs-git-modified-face :foreground blue)))

;;; hacker-theme.el ends here
