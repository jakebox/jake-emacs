;;; jake-doom-plain-classy-theme.el --- inspired by gko's plain theme for VSCode


(require 'doom-themes)

(defgroup jake-doom-plain-classy-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom jake-doom-plain-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'jake-doom-plain-classy-theme
  :type 'boolean)

(defcustom jake-doom-plain-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to

determine the exact padding."
  :group 'jake-doom-plain-classy-theme
  :type '(or integer boolean))

(def-doom-theme jake-doom-plain-classy
  "Theme inspired by gko's plain."

  ;; name        default   256       16
  ((bg         '("#ffffff" nil       nil ))
   (bg-alt     '("#f3f3f3" nil       nil ))
   (base0      '("#969896" nil nil ))
   (base1      '("#f1f3f5" nil nil ))
   (base2      '("#363636" nil nil ))
   (base2-1    '("#575757" nil nil ))
   (base3      '("#cccccc" nil nil ))
   (base4      '("#e7e7e7" nil nil ))
   (base5      '("#c5c8c6" nil nil ))
   (base6      '("#fafafa" nil nil ))
   (base7      '("#dfdfdf" nil nil ))
   (base8      '("#fafafa" nil nil ))
   (fg         '("#282a2e" nil nil ))
   (fg-alt     (doom-lighten fg 0.15))

   (grey       fg)
   (red        fg)
   (blue       fg)
   (dark-blue  fg)
   (orange     fg)
   (green      fg)
   (teal       fg)
   (yellow     fg)
   (magenta    fg)
   (violet     fg)
   (cyan       fg)
   (dark-cyan  fg)

   ;; face categories -- required for all themes
   (highlight      base2)
   (vertical-bar   (doom-lighten fg 0.3))
   (selection      base1)
   (builtin        base0)
   (comments       base5)
   (doc-comments   base5)
   (constants      base0)
   (functions      fg)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        base0)
   (variables      base0)
   (numbers        base0)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright jake-doom-plain-brighter-modeline)
   (-modeline-pad
    (when jake-doom-plain-padded-modeline
      (if (integerp jake-doom-plain-padded-modeline) jake-doom-plain-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  (
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base2)

   (hl-line
    :background base8)

   (header-line
    :background nil)

   (button
    :foreground "#0000c0")

   (org-link
    :inherit 'button
    :underline t)

   (org-drawer
	:height 0.6
	:inherit 'shadow)

   (org-scheduled-today
    :foreground base2)
   
   (org-block-begin-line
    :foreground base2
    :background base3)

   (org-block-end-line
    :foreground base2
    :background base3)

   (org-block
    :background "#f7f7f7"
    :extend t)

   (org-document-title
	:height 1.3
	:weight 'bold
	:foreground fg)
   
   ;; The bullets are just slightly lighter/more subtle than the header text.
   (org-superstar-header-bullet
    :foreground "#7a7a7a"
    :background nil)

   (org-warning
    :foreground "#941212"
    :background nil
    :weight 'bold)
   
   (org-level-1
    :slant 'normal
    :foreground fg-alt
    :weight 'bold
    :height 1.5
    :background nil)

   (org-level-2
    :slant 'normal
    :foreground base2
    :weight 'bold
    :height 1.3
    :background nil)

   (org-level-3
    :slant 'normal
    :foreground base2
    :weight 'bold
    :height 1.2
    :background nil)

   ;; Org levels 4 and 5 are slightly lighter in color than the preceding levels.
   (org-level-4
    :slant 'normal
    :foreground base2-1
    :background nil)

   (org-level-5
    :slant 'normal
    :foreground base2-1
    :background nil)

   ;; Font lock
   (font-lock-comment-face
    :foreground comments
    :slant 'italic)
   (font-lock-type-face
    :foreground type
    :slant 'italic)
   (font-lock-function-name-face
    :foreground functions
    :slant 'italic)
   (font-lock-doc-face
    :foreground doc-comments
    :slant 'italic)
   (font-lock-constant-face
    :foreground constants
    :slant 'italic)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   ;; LSP
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; jake-doom-plain-classy-theme.el ends here
