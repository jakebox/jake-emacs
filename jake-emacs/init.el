;; -*- lexical-binding: t; -*-
;;; 
;;; Jake B's Emacs Configuration
;;;

;; Copyright (C) Jake B
;; Author: Jake B <jakebox0@protonmail.com>
;; URL: https://github.com/jakebox/.emacs
;; This file is not part of GNU Emacs.
;; This file is free software.

;; ------- The following code was auto-tangled from an Orgmode file. ------- ;;

(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; ELPA and NonGNU ELPA are default in Emacs28
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) 
;; Without this on my Mac Emacs freezes when refreshing ELPA
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") 

;; Package list
(setq package-list '(

                     ;; Emacs
                     use-package gcmh

                     ;; Completion/Utilities
                     company counsel ivy prescient company-prescient
                     ivy-prescient

                     ;; Utilities/Modes
                     auctex projectile web-mode python-mode pyvenv
                     auto-virtualenv

                     ;; QOL (Quality-of-life) & "Utilities"
                     saveplace super-save simpleclip centered-cursor-mode
                     undo-fu yasnippet super-save ace-window windresize unfill
                     rainbow-mode popper burly

                     deft define-word mw-thesaurus mu4e-views restart-emacs
                     unfill mu4e-views reveal-in-osx-finder pdf-tools

                     ;; Keyboard
                     evil which-key general smartparens hydra evil-surround
                     evil-snipe evil-org evil-anzu evil-collection

                     ;; Themes/Visuals
                     modus-themes doom-themes kaolin-themes dashboard
                     solaire-mode mixed-pitch visual-fill-column diminish
                     doom-modeline hide-mode-line writeroom-mode all-the-icons
                     all-the-icons-ivy-rich presentation

                     ;; Org-related
                     ox-reveal ox-hugo org-superstar org-super-agenda org-gcal
                     toc-org org-ql org-appear org-ql

                     htmlize
                     ))


(package-initialize)
(setq package-enable-at-startup nil)

;; Install packages that aren't installed
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless
      (package-installed-p package) (package-install package)))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar jib/home (concat (getenv "HOME") "/") "My home directory.")
(defvar jib/computer 'laptop "Which computer I am on -- 'desktop or 'laptop.")
(defvar jib-text-height nil "My preferred default text height.")
(defvar jib-doom-modeline-text-height nil "My preferred modeline text height.")
(defvar jib-default-line-spacing 0 "Baseline line spacing.")
(setq-default jib-default-line-spacing 0)

;; Referring to external (Dropbox) locations/files
(defvar jib/dropbox (concat jib/home "Dropbox/") "The parent Dropbox folder.")
(defvar org-directory (concat jib/dropbox "org") "Directory with org files.")
(defvar jib/emacs-stuff (concat jib/dropbox "Files/systems/emacs-stuff") "Dropbox directory where all Emacs files are kept")

;; At this point things can be local now
(setq jib/init.org (expand-file-name "init.org" user-emacs-directory))
(setq bookmark-default-file (concat jib/emacs-stuff "/bookmarks"))
(setq custom-theme-directory (expand-file-name "themes" jib/emacs-stuff))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculated variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set `jib/computer' to 'laptop or 'desktop.
(let ((sys (system-name)))
  (if (or (or (string= sys "MJBs-MacBook-Air.local") (string= sys "MJBs-Air.fwparker.org")) (string= sys "mjbs-air.lan"))
      (setq jib/computer 'laptop)
    (setq jib/computer 'desktop)))

(load (expand-file-name "jib-funcs.el" user-emacs-directory))
(load (expand-file-name "private.el" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load custom-file)

;; I put mostly stuff I find online in this "lisp" folder in my emacs-stuff.
;; Add every directory in that folder to the load-path.
(let ((default-directory (directory-file-name (concat jib/emacs-stuff "/lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

(setq register-preview-delay 0) ;; Show registers ASAP

(set-register ?i (cons 'file (concat org-directory "/cpb.org")))
(set-register ?h (cons 'file (concat org-directory "/work.org")))
(set-register ?C (cons 'file (concat jib/emacs-stuff "/jake-emacs/init.org")))
(set-register ?A (cons 'file (concat org-directory "/org-archive/homework-archive.org_archive")))
(set-register ?T (cons 'file (concat org-directory "/org-archive/todo-archive.org_archive")))

(setq exec-path '("/usr/local/Cellar/pyenv-virtualenv/1.1.5/shims"
                  "/Users/jake/.pyenv/shims" "/usr/local/bin" "/bin"
                  "/usr/bin" "/usr/sbin" "/usr/local/sbin" "/sbin"
                  "/Users/jake/bin" "/Users/jake/doom-emacs/bin"
                  "/Library/TeX/texbin"))

(setenv "PATH" "/usr/local/Cellar/pyenv-virtualenv/1.1.5/shims:/Users/jake/.pyenv/shims:/usr/local/bin:/bin:/usr/bin:/usr/sbin:/usr/local/sbin:/sbin:/Users/jake/bin:/Users/jake/doom-emacs/bin:/Library/TeX/texbin")

;; A cool mode to revert window configurations.
(winner-mode 1)

;; INTERACTION -----

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; Confirm to quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Major mode of new buffers
(setq initial-major-mode 'org-mode)

;; WINDOW -----------

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; When opening a file (like double click) on Mac, use an existing frame
(setq ns-pop-up-frames nil)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; where we resize windows too quickly.
(setq window-resize-pixelwise nil)

;; LINES -----------
(setq-default truncate-lines t)

(setq-default tab-width 4)

(setq-default fill-column 80)

(use-package paren
  ;; highlight matching delimiters
  :ensure nil
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1))


(setq sentence-end-double-space nil) ;; Sentences end with one space

(setq bookmark-fontify nil)

;; SCROLLING ---------
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil)))
(setq scroll-conservatively 101)


(setq
 ;; If the frame contains multiple windows, scroll the one under the cursor
 ;; instead of the one that currently has keyboard focus.
 mouse-wheel-follow-mouse 't
 ;; Completely disable mouse wheel acceleration to avoid speeding away.
 mouse-wheel-progressive-speed nil
 ;; The most important setting of all! Make each scroll-event move 2 lines at
 ;; a time (instead of 5 at default). Simply hold down shift to move twice as
 ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
 mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))

;; sane trackpad/mouse scroll settings (doom)
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

(setq visible-bell nil) ;; Make it ring (so no visible bell) (default)
(setq ring-bell-function 'ignore) ;; BUT ignore it, so we see and hear nothing

(setq line-move-visual t) ;; C-p, C-n, etc uses visual lines

;; Blank scratch buffer
(setq initial-scratch-message "")

;; Uses system trash rather than deleting forever
(setq trash-directory (concat jib/home ".Trash"))
(setq delete-by-moving-to-trash t)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; When opening a symlink that links to a file in a git repo, edit the file in the
;; git repo so we can use the Emacs vc features (like Diff) in the future
(setq vc-follow-symlinks t)

;; BACKUPS/LOCKFILES --------
;; Don't generate backups or lockfiles.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))

(use-package recentf
  :ensure nil
  :config
  (setq ;;recentf-auto-cleanup 'never
          ;; recentf-max-menu-items 0
          recentf-max-saved-items 200)
  (recentf-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ENCODING -------------
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

(setq default-input-method "spanish-postfix") ;; When I need to type in Spanish (switch with C-\)

(setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
(setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

(setq blink-cursor-interval 0.6)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; (setq echo-keystrokes 0.8)



(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; Weird thing where `list-colors-display` doesn't show all colors.
;; https://bug-gnu-emacs.gnu.narkive.com/Bo6OdySs/bug-5683-23-1-93-list-colors-display-doesn-t-show-all-colors
(setq x-colors (ns-list-colors))

;; How thin the window should be to stop splitting vertically (I think)
(setq split-width-threshold 80)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-prefix-prefix "◉ ")
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 2
        which-key-max-display-columns 4))

(use-package evil
  :init
  ;; (setq evil-want-keybinding t)
  (setq evil-want-fine-undo t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  :config

  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)

  ;; ----- Keybindings
  ;; I tried using evil-define-key for these. Didn't work.
  ;; (define-key evil-motion-state-map "/" 'swiper)
  (define-key evil-window-map "\C-q" 'evil-delete-buffer) ;; Maps C-w C-q to evil-delete-buffer (The first C-w puts you into evil-window-map)
  (define-key evil-window-map "\C-w" 'kill-this-buffer)
  (define-key evil-motion-state-map "\C-b" 'evil-scroll-up) ;; Makes C-b how C-u is

  ;; ----- Setting cursor colors
  (setq evil-emacs-state-cursor    '("#649bce" box))
  (setq evil-normal-state-cursor   '("#ebcb8b" box))
  (setq evil-operator-state-cursor '("#ebcb8b" hollow))
  (setq evil-visual-state-cursor   '("#677691" box))
  (setq evil-insert-state-cursor   '("#eb998b" (bar . 2)))
  (setq evil-replace-state-cursor  '("#eb998b" hbar))
  (setq evil-motion-state-cursor   '("#ad8beb" box))

  ;; ;; Evil-like keybinds for custom-mode-map
  ;; (evil-define-key nil 'custom-mode-map
  ;;   ;; motion
  ;;   (kbd "C-j") 'widget-forward
  ;;   (kbd "C-k") 'widget-backward
  ;;   "q" 'Custom-buffer-done)

  (evil-mode 1))

(use-package evil-surround
  :defer 2
  :config
  (global-evil-surround-mode 1))


(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) calc diff-mode))
  (evil-collection-init)
  ;; A few of my own overrides/customizations
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "RET") 'dired-find-alternate-file)

  (evil-define-key 'motion 'dired-mode-map "Q" 'kill-this-buffer)
  (evil-define-key 'motion help-mode-map "q" 'kill-this-buffer)
  (evil-define-key 'motion calendar-mode-map "q" 'kill-this-buffer))

(use-package evil-snipe
  :diminish evil-snipe-mode
  :diminish evil-snipe-local-mode
  :after evil
  :config
  (evil-snipe-mode +1))

(use-package general
  :config

(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"

 ;; Top level functions
 "/" '(counsel-rg :which-key "ripgrep")
 ";" '(spacemacs/deft :which-key "deft")
 ":" '(projectile-find-file :which-key "p-find file")
 "." '(counsel-find-file :which-key "find file")
 "," '(counsel-recentf :which-key "recent files")
 "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
 "SPC" '(counsel-M-x :which-key "M-x")
 "q" '(save-buffers-kill-terminal :which-key "quit emacs")
 "r" '(jump-to-register :which-key "registers")

;; "Applications"
"a" '(nil :which-key "applications")
"ao" '(org-agenda :which-key "org-agenda")
"am" '(mu4e :which-key "mu4e")
"aC" '(calc :which-key "calc")
"ac" '(org-capture :which-key "org-capture")
"aqq" '(org-ql-view :which-key "org-ql-view")
"aqs" '(org-ql-search :which-key "org-ql-search")

"ab" '(nil :which-key "browse url")
"abf" '(browse-url-firefox :which-key "firefox")
"abc" '(browse-url-chrome :which-key "chrome")
"abx" '(xwidget-webkit-browse-url :which-key "xwidget")

"ad" '(dired :which-key "dired")

;; Buffers
"b" '(nil :which-key "buffer")
"bb" '(counsel-switch-buffer :which-key "switch buffers")
"bd" '(evil-delete-buffer :which-key "delete buffer")
"bs" '(jib/switch-to-scratch-buffer :which-key "scratch buffer")
"bm" '(jib/kill-other-buffers :which-key "kill other buffers")
"bi" '(clone-indirect-buffer  :which-key "indirect buffer")
"br" '(revert-buffer :which-key "revert buffer")

;; Files
"f" '(nil :which-key "files")
"fb" '(counsel-bookmark :which-key "bookmarks")
"ff" '(counsel-find-file :which-key "find file")
"fn" '(spacemacs/new-empty-buffer :which-key "new file")
"fr" '(counsel-recentf :which-key "recent files")
"fR" '(rename-file :which-key "rename file")
"fs" '(save-buffer :which-key "save buffer")
"fS" '(evil-write-all :which-key "save all buffers")
"fo" '(reveal-in-osx-finder :which-key "reveal in finder")
"fO" '(jib/open-buffer-file-mac :which-key "open buffer file")

;; Jake
"j" '(nil :which-key "jake")
"jb" '((lambda() (interactive)(find-file (concat jib/dropbox "org/work.org"))) :which-key "work.org")
"jc" '((lambda() (interactive)(find-file (concat jib/dropbox "org/cpb.org"))) :which-key "cpb.org")

"jr" '(restart-emacs :which-key "restart emacs")

"jh" '(nil :which-key "hydras")
"jht" '(jib-hydra-theme-switcher/body :which-key "themes")
"jhf" '(jib-hydra-variable-fonts/body :which-key "mixed-pitch face")
"jhw" '(jib-hydra-window/body :which-key "window control")

"jm" '(nil :which-key "macros/custom commands")
"jml" '(jib/listify :which-key "Listify")
"jmL" '(jib|SubListify :which-key "SubListify")
"jmo" '(jib/org-temp-export-html :which-key "org temp export region")

"jk" '(nil :which-key "agenda/ql")
"jkq" '((lambda () (interactive) (org-ql-view "Jake Work Full View")) :which-key "jake ql")

;; Help/emacs
"h" '(nil :which-key "help/emacs")

"hv" '(counsel-describe-variable :which-key "des. variable")
"hb" '(counsel-descbinds :which-key "des. bindings")
"hM" '(describe-mode :which-key "des. mode")
"hf" '(counsel-describe-function :which-key "des. func")
"hF" '(counsel-describe-face :which-key "des. face")
"hk" '(describe-key :which-key "des. key")

"hed" '(jib/edit-init :which-key "edit dotfile")

"hm" '(nil :which-key "switch mode")
"hme" '(emacs-lisp-mode :which-key "elisp mode")
"hmo" '(org-mode :which-key "org mode")
"hmt" '(text-mode :which-key "text mode")

;; Help/emacs
"x" '(nil :which-key "text")
"xC" '(jib/copy-whole-buffer-to-clipboard :which-key "copy whole buffer to clipboard")
"xr" '(anzu-query-replace :which-key "find and replace")
"xs" '(yas-insert-snippet :which-key "insert yasnippet")

;; Toggles
"t" '(nil :which-key "toggles")
"tT" '(toggle-truncate-lines :which-key "truncate lines")
"tv" '(visual-line-mode :which-key "visual line mode")
"tn" '(display-line-numbers-mode :which-key "display line numbers")
"ta" '(mixed-pitch-mode :which-key "variable pitch mode")
"tc" '(visual-fill-column-mode :which-key "visual fill column mode")
"tt" '(counsel-load-theme :which-key "load theme")
"tw" '(writeroom-mode :which-key "writeroom-mode")
"tR" '(read-only-mode :which-key "read only mode")
"tI" '(toggle-input-method :which-key "toggle input method")
"tr" '(display-fill-column-indicator-mode :which-key "fill column indicator")
"tm" '(hide-mode-line-mode :which-key "hide modeline mode")

;; Windows
"w" '(nil :which-key "window")
"wm" '(jib/toggle-maximize-buffer :which-key "maximize buffer")
"wN" '(make-frame :which-key "make frame")
"wd" '(evil-window-delete :which-key "delete window")
"w-" '(jib/split-window-vertically-and-switch :which-key "split below")
"w/" '(jib/split-window-horizontally-and-switch :which-key "split right")
"wr" '(jib-hydra-window/body :which-key "hydra window")
"wl" '(evil-window-right :which-key "evil-window-right")
"wh" '(evil-window-left :which-key "evil-window-left")
"wj" '(evil-window-down :which-key "evil-window-down")
"wk" '(evil-window-up :which-key "evil-window-up")
"wz" '(text-scale-adjust :which-key "text zoom")
) ;; End SPC prefix general.el block

(general-def
  :prefix ","
  :states 'motion
  :keymaps 'emacs-lisp-mode-map
  "" nil
  "e" '(nil :which-key "eval")
  "es" '(eval-last-sexp :which-key "eval-sexp")
  "er" '(eval-region :which-key "eval-region")
  "eb" '(eval-buffer :which-key "eval-buffer")


  "g" '(counsel-imenu :which-key "imenu")
  "c" '(check-parens :which-key "check parens")
  "I" '(indent-region :which-key "indent-region")
  )

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "t" 'org-todo
  "<return>" 'org-open-at-point-global
  "K" 'org-shiftup
  "J" 'org-shiftdown
  )

(general-def
  :states '(normal insert)
  :keymaps 'org-mode-map
  "C-c h" 'org-html-export-to-html
  "M-[" 'org-metaleft
  "M-]" 'org-metaright
  "C-M-=" 'ap/org-count-words
  )

;; Org-src - when editing an org source block
(general-def
  :prefix ","
  :states 'normal
  :keymaps 'org-src-mode-map
  "b" '(nil :which-key "org src")
  "bc" 'org-edit-src-abort
  "bb" 'org-edit-src-exit
  )

(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-mode-map) ;; Available in org mode, org agenda
 "" nil
 "A" '(org-archive-subtree-default :which-key "org-archive")
 "a" '(org-agenda :which-key "org agenda")
 "6" '(org-sort :which-key "sort")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-schedule :which-key "schedule")
 "S" '(jib/org-schedule-tomorrow :which-key "schedule")
 "d" '(org-deadline :which-key "deadline")
 "g" '(counsel-org-goto :which-key "goto heading")
 "t" '(counsel-org-tag :which-key "set tags")
 "p" '(org-set-property :which-key "set property")
 "e" '(org-export-dispatch :which-key "export org")
 "B" '(org-toggle-narrow-to-subtree :which-key "toggle narrow to subtree")
 "V" '(jib/org-set-startup-visibility :which-key "startup visibility")
 "H" '(org-html-convert-region-to-html :which-key "convert region to html")

 ;; org-babel
 "b" '(nil :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle")
 "bb" '(org-edit-special :which-key "org-edit-special")
 "bc" '(org-edit-src-abort :which-key "org-edit-src-abort")

 "x" '(nil :which-key "text")
 "xb" (spacemacs|org-emphasize jib/org-bold ?*)
 "xb" (spacemacs|org-emphasize jib/org-bold ?*)
 "xc" (spacemacs|org-emphasize jib/org-code ?~)
 "xi" (spacemacs|org-emphasize jib/org-italic ?/)
 "xs" (spacemacs|org-emphasize jib/org-strike-through ?+)
 "xu" (spacemacs|org-emphasize jib/org-underline ?_)
 "xv" (spacemacs|org-emphasize jib/org-verbose ?~) ;; I realized that ~~ is the same and better than == (Github won't do ==)

 ;; insert
 "i" '(nil :which-key "insert")

 "it" '(nil :which-key "tables")
 "itt" '(org-table-create :which-key "create table")
 "itl" '(org-table-insert-hline :which-key "table hline")

 "il" '(org-insert-link :which-key "link")

 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-clock-in :which-key "clock in")
 "co" '(org-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )


(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-agenda-mode-map) ;; Available in org mode, org agenda
 "" nil
 "a" '(org-agenda :which-key "org agenda")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-agenda-schedule :which-key "schedule")
 "d" '(org-agenda-deadline :which-key "deadline")
 "t" '(org-agenda-set-tags :which-key "set tags")
 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-agenda-clock-in :which-key "clock in")
 "co" '(org-agenda-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )

;; All-mode keymaps
(general-def
  :keymaps 'override

  ;; Emacs --------
  "M-x" 'counsel-M-x
  "ß" 'evil-window-next ;; option-s
  "Í" 'other-frame ;; option-shift-s
  "C-S-B" 'counsel-switch-buffer
  "∫" 'counsel-switch-buffer ;; option-b
  "s-o" 'jib-hydra-window/body

  ;; Remapping normal help features to use Counsel version
  "C-h v" 'counsel-describe-variable
  "C-h o" 'counsel-describe-symbol
  "C-h f" 'counsel-describe-function
  "C-h F" 'counsel-describe-face

  ;; Editing ------
  "M-v" 'simpleclip-paste
  "M-V" 'evil-paste-after ;; shift-paste uses the internal clipboard
  "M-c" 'simpleclip-copy
  "M-u" 'capitalize-dwim ;; Default is upcase-dwim
  "M-U" 'upcase-dwim ;; M-S-u (switch upcase and capitalize)
  "C-c u" 'jib/split-and-close-sentence

  ;; Utility ------
  "C-c c" 'org-capture
  "C-c a" 'org-agenda
  "C-s" 'counsel-grep-or-swiper ;; Large files will use grep (faster)
  "s-\"" 'ispell-word ;; that's super-shift-'
  "M-+" 'jib/calc-speaking-time

  ;; super-number functions
  "s-1" 'mw-thesaurus-lookup-dwim
  "s-2" 'ispell-buffer
  "s-3" 'revert-buffer
  "s-4" '(lambda () (interactive) (counsel-file-jump nil jib/dropbox))
  )

;; Non-insert mode keymaps
(general-def
  :states '(normal visual motion)
  "gc" 'comment-dwim
  "j" 'evil-next-visual-line ;; I prefer visual line navigation
  "k" 'evil-previous-visual-line ;; ""
  "|" '(lambda () (interactive) (org-agenda nil "n")) ;; Opens my n custom org-super-agenda view
  "C-|" '(lambda () (interactive) (org-agenda nil "m")) ;; Opens my m custom org-super-agenda view
  )

;; Insert keymaps
;; Many of these are emulating standard Emacs bindings in Evil insert mode, such as C-a, or C-e.
(general-def
  :states '(insert)
  "C-a" 'evil-beginning-of-visual-line
  "C-e" 'evil-end-of-visual-line
  "C-S-a" 'evil-beginning-of-line
  "C-S-e" 'evil-end-of-line
  "C-n" 'evil-next-visual-line
  "C-p" 'evil-previous-visual-line
  )

;; Xwidget ------
(general-define-key :states 'normal :keymaps 'xwidget-webkit-mode-map 
                    "j" 'xwidget-webkit-scroll-up-line
                    "k" 'xwidget-webkit-scroll-down-line
                    "gg" 'xwidget-webkit-scroll-top
                    "G" 'xwidget-webkit-scroll-bottom)

;; 'q' kills help buffers rather than just closing the window
;; (general-define-key :keymaps '(help-mode-map calendar-mode-map) "q" 'kill-this-buffer)

) ;; end general.el use-package

(use-package hydra
  :defer t)

;; This Hydra lets me swich between variable pitch fonts. It turns off mixed-pitch 
;; WIP
(defhydra jib-hydra-variable-fonts (:pre (mixed-pitch-mode 0)
                                     :post (mixed-pitch-mode 1))
  ("t" (set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 160) "Times New Roman")
  ("g" (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160 :weight 'normal) "EB Garamond")
  ;; ("r" (set-face-attribute 'variable-pitch nil :font "Roboto" :weight 'medium :height 160) "Roboto")
  ("n" (set-face-attribute 'variable-pitch nil :slant 'normal :weight 'normal :height 160 :width 'normal :foundry "nil" :family "Nunito") "Nunito")
  )

(defun jib/load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defhydra jib-hydra-theme-switcher (:hint nil)
  "
     Dark                ^Light^
----------------------------------------------
_1_ one              _z_ one-light 
_2_ vivendi          _x_ operandi
_3_ molokai          _c_ jake-plain
_4_ snazzy           _v_ flatwhite
_5_ old-hope         _b_ opera-light 
_6_ henna                ^
_7_ kaolin-galaxy        ^
_8_ peacock              ^
_9_ jake-plain-dark      ^
_q_ quit                 ^
^                        ^
"

  ;; Dark
  ("1" (jib/load-theme 'doom-one) "one")
  ("2" (jib/load-theme 'modus-vivendi) "modus-vivendi")
  ("3" (jib/load-theme 'doom-molokai) "molokai")
  ("4" (jib/load-theme 'doom-snazzy) "snazzy")
  ("5" (jib/load-theme 'doom-old-hope) "old-hope")
  ("6" (jib/load-theme 'doom-henna) "henna")
  ("7" (jib/load-theme 'kaolin-galaxy) "kaolin-galaxy")
  ("8" (jib/load-theme 'doom-peacock) "peacock")
  ("9" (jib/load-theme 'jake-doom-plain-dark) "jake-plain-dark")

  ;; Light
  ("z" (jib/load-theme 'doom-one-light) "one-light")
  ("x" (jib/load-theme 'modus-operandi) "modus-operandi")
  ("c" (jib/load-theme 'jake-doom-plain) "jake-plain")
  ("v" (jib/load-theme 'doom-flatwhite) "flatwhite")
  ("b" (jib/load-theme 'doom-opera-light) "opera-light")
  ("q" nil))

;; I think I need to initialize windresize to use its commands
(windresize)
(windresize-exit)

;; All-in-one window managment. Makes use of some custom functions,
;; `ace-window' (for swapping), `windmove' (could probably be replaced
;; by evil?) and `windresize'.
;; inspired by https://github.com/jmercouris/configuration/blob/master/.emacs.d/hydra.el#L86
(defhydra jib-hydra-window (:hint nil)
   "
Movement      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------------
_M-<left>_  <   _/_ vertical      _b_uffer        _<left>_  <
_M-<right>_ >   _-_ horizontal    _f_ind file     _<down>_  ↓
_M-<up>_    ↑   _m_aximize        _s_wap          _<up>_    ↑
_M-<down>_  ↓   _c_lose           _[_backward     _<right>_ >
_q_uit          _e_qualize        _]_forward     ^
^               ^               _K_ill         ^
^               ^                  ^             ^
"
   ;; Movement
   ("M-<left>" windmove-left)
   ("M-<down>" windmove-down)
   ("M-<up>" windmove-up)
   ("M-<right>" windmove-right)

   ;; Split/manage
   ("-" jib/split-window-vertically-and-switch)
   ("/" jib/split-window-horizontally-and-switch)
   ("c" evil-window-delete)
   ("d" evil-window-delete)
   ("m" delete-other-windows)
   ("e" balance-windows)

   ;; Switch
   ("b" counsel-switch-buffer)
   ("f" counsel-find-file)
   ("P" projectile-find-file)
   ("s" ace-swap-window)
   ("[" previous-buffer)
   ("]" next-buffer)
   ("K" kill-this-buffer)

   ;; Resize
   ("<left>" windresize-left)
   ("<right>" windresize-right)
   ("<down>" windresize-down)
   ("<up>" windresize-up)


   ("q" nil))

(use-package company
  :diminish company-mode
  :general
  (general-define-key :keymaps 'company-active-map
                      "C-j" 'company-select-next
                      "C-k" 'company-select-previous)
  :init
  ;; These configurations come from Doom Emacs:
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area
        company-backends '(company-capf company-files company-keywords)
        company-auto-complete nil
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  :config
  (setq company-idle-delay 0.35)
  :custom-face
  (company-tooltip ((t (:family "Roboto Mono")))))


;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   :init
;;   (setq company-box-icons-alist 'company-box-icons-all-the-icons)
;;   (setq company-box-icons-elisp
;;    '((fa_tag :face font-lock-function-name-face) ;; Function
;;      (fa_cog :face font-lock-variable-name-face) ;; Variable
;;      (fa_cube :face font-lock-constant-face) ;; Feature
;;      (md_color_lens :face font-lock-doc-face))) ;; Face
;;   :config
;;   (require 'all-the-icons)
;;   (setf (alist-get 'min-height company-box-frame-parameters) 6)
;;   (setq company-box-icons-alist 'company-box-icons-all-the-icons)
;;   )

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-extra-directories nil) ;; Hides . and .. directories
  (setq ivy-initial-inputs-alist nil) ;; Removes the ^ in ivy searches
  (if (eq jib/computer 'laptop)
      (setq-default ivy-height 10)
    (setq-default ivy-height 15))
  (setq ivy-fixed-height-minibuffer t)
  (ivy-mode 1)

  ;; Shows a preview of the face in counsel-describe-face
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))

  :general
  (general-define-key
   ;; Also put in ivy-switch-buffer-map b/c otherwise switch buffer map overrides and C-k kills buffers
   :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
   ;; C-j and C-k to move up/down in Ivy
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line)
  )

;; Nice icons in Ivy. Replaces all-the-icons-ivy.
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  :config
  (setq all-the-icons-ivy-rich-icon-size 1.0))

(use-package ivy-rich
  :after (ivy)
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (setq default-directory jib/home)
  (setq counsel-switch-buffer-preview-virtual-buffers nil) ;; Removes recentfiles/bookmarks from counsel-switch-buffer
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; That weird Icon? file in Dropbox.
         "\\(Icon\\\)"
         ;; Hides file names beginning with # or .
         "\\|\\(?:\\`[#.]\\)"))

  ;; emacs regexp notes: had to put \\| before the second regexp to make this work

  ;; Sorts counsel-recentf in order of time last accessed
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-recentf . file-newer-than-file-p))

  (add-to-list 'recentf-exclude
               (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

  (setq-default counsel--fzf-dir jib/home)
  :general
  (general-define-key :keymaps 'counsel-find-file-map
                      "C-c f" 'counsel-file-jump-from-find) ;; when in counsel-find-file, run this to search the whole directory recursively
  )

(use-package prescient
  :config
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000) ;; More prescient history
  (prescient-persist-mode +1))

;; Use `prescient' for Ivy menus.
(use-package ivy-prescient
  :after ivy
  :config
  ;; don't prescient sort these commands
  (dolist (command '(org-ql-view counsel-find-file))
    (setq ivy-prescient-sort-commands (append ivy-prescient-sort-commands (list command))))
  (ivy-prescient-mode +1))

(use-package company-prescient
  :defer 2
  :after company
  :config
  (company-prescient-mode +1))

(use-package smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (with-eval-after-load 'evil
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))


  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (smartparens-global-mode t))

(use-package flyspell
  :defer t
  :config
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))

  (dolist (mode '(org-mode-hook
                  mu4e-compose-mode-hook))
    (add-hook mode (lambda () (flyspell-mode 1))))
  :general ;; Switches correct word from middle click to right click
  (general-define-key :keymaps 'flyspell-mouse-map
                      "<mouse-3>" #'flyspell-correct-word
                      "<mouse-2>" nil)
  )

(use-package evil-anzu :defer t)

(use-package simpleclip
  :config
  (simpleclip-mode 1))
;; Allows pasting in minibuffer with M-v
(add-hook 'minibuffer-setup-hook 'jib/paste-in-minibuffer)



(defun jib/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (mark-whole-buffer)
  (simpleclip-copy (point-min) (point-max))
  (deactivate-mark))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo))

(use-package super-save
  :diminish super-save-mode
  :defer 2
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 5) ;; after 5 seconds of not typing autosave
  (setq super-save-triggers ;; Functions after which buffers are saved (switching window, for example)
        '(evil-window-next evil-window-prev balance-windows other-window))
  (super-save-mode +1))

;; After super-save autosaves, wait __ seconds and then clear the buffer. I don't like
;; the save message just sitting in the echo area.
(defun jib-clear-echo-area-timer ()
  (run-at-time "2 sec" nil (lambda () (message " "))))

(advice-add 'super-save-command :after 'jib-clear-echo-area-timer)

(use-package saveplace
  :init (setq save-place-limit 100)
  :config (save-place-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 5
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" jib/emacs-stuff)))
  (yas-global-mode 1)) ;; or M-x yas-reload-all if you've started YASnippet already.


;; Silences the warning when running a snippet with backticks (runs a command in the snippet)
;; I use backtick commands to get the date for org snippets
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(setq text-scale-mode-step 1.1) ;; How much to adjust text scale by when using `text-scale-mode'
(setq jib-default-line-spacing 0) ;; This happens in the variables but I guess I have it here too.

(setq-default line-spacing jib-default-line-spacing)

;; Setting text size based on the computer I am on.
(if (eq jib/computer 'laptop)
    (setq jib-text-height 140))
(if (eq jib/computer 'desktop)
    (setq jib-text-height 150))

(set-face-attribute 'default nil :family "Roboto Mono" :weight 'regular :height jib-text-height)

;; Float height value (1.0) makes fixed-pitch take height 1.0 * height of default
;; This means it will scale along with default when the text is zoomed
(set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :weight 'regular :height 1.0)

;; Height of 160 seems to match perfectly with 12-point on Google Docs
(set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 160)

(use-package mixed-pitch
  :defer t
  :config
  (setq mixed-pitch-set-height t)
  (dolist (face '(org-date org-priority org-tag org-special-keyword)) ;; Some extra faces I like to be fixed-pitch
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(defun my-presentation-on ()
  (setq jib-default-line-spacing 3)

  (setq-default line-spacing jib-default-line-spacing)
  (setq-local line-spacing jib-default-line-spacing)

  (setq ivy-height 5))

(defun my-presentation-off ()
  (jib/reset-var 'jib-default-line-spacing)
  (setq-default line-spacing jib-default-line-spacing)
  (setq-local line-spacing jib-default-line-spacing)
  (jib/reset-var 'ivy-height))

(add-hook 'presentation-on-hook #'my-presentation-on)
(add-hook 'presentation-off-hook #'my-presentation-off)

(if (eq jib/computer 'laptop)
    (setq presentation-default-text-scale 4)
  (setq presentation-default-text-scale 5))

(use-package presentation
  :defer t)

;; Disables showing system load in modeline, useless anyway
(setq display-time-default-load-average nil)

(line-number-mode)
(column-number-mode)
(display-time-mode -1)
(size-indication-mode -1)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name ;; Just show file name (no path)
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
        doom-modeline-icon t ;; Enable/disable all icons
        doom-modeline-modal-icon nil ;; Icon for Evil mode
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-bar-width 3))

;; Configure modeline text height based on the computer I'm on.
;; These variables are used in the Themes section to ensure the modeline
;; stays the right size no matter what theme I use.
(if (eq jib/computer 'laptop)
    (setq jib-doom-modeline-text-height 135) ;; If laptop
  (setq jib-doom-modeline-text-height 140))  ;; If desktop

(if (eq jib/computer 'laptop)
    (setq doom-modeline-height 1) ;; If laptop
  (setq doom-modeline-height 1))  ;; If desktop

;; Window's initial size and a bit of border
(if (eq jib/computer 'laptop)
    (setq default-frame-alist '((left . 150)
                                (width . 120)
                                (fullscreen . fullheight)
                                ;; (vertical-scroll-bars . nil) ;; Think this isn't needed
                                (internal-border-width . 8))))

(if (eq jib/computer 'desktop)
    (setq default-frame-alist '((left . 170)
                                (width . 173)
                                (top . 64)
                                (height . 53)
                                (fullscreen . fullheight)
                                (internal-border-width . 8))))

;; (frame-parameter nil 'left)

(use-package all-the-icons) 

(use-package doom-themes
  :after mixed-pitch
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom-face
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,jib-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,jib-doom-modeline-text-height))))
  (org-scheduled-previously ((t (:background "red")))))

(use-package kaolin-themes
  :config
  (setq kaolin-themes-modeline-border nil)
  :custom-face
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,jib-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,jib-doom-modeline-text-height))))

  ;; Disable underline for org deadline warnings. I don't like the way it looks.
  (org-warning ((t (:underline nil))))

  ;; Darkens the org-ellipsis (first unset the color, then give it shadow)
  (org-ellipsis ((t (:foreground unspecified :inherit 'shadow)))))

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
        modus-themes-hl-line '(accented) 
        modus-themes-syntax '(yellow-comments)
        modus-themes-mode-line '(accented borderless)) ;; Color modeline in active window, remove border
  (setq modus-themes-headings ;; Makes org headings more colorful
        '((t . (rainbow))))
  (modus-themes-load-themes)
  :custom-face
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,jib-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,jib-doom-modeline-text-height)))))


;; Loading theme based on the time.
(let ((hour (string-to-number (substring (current-time-string) 11 13))))
  (if (or (> hour 16) (< hour 7))
      (load-theme 'doom-one t) ;; Night
    (load-theme 'doom-opera-light t))) ;; Day

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil) ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'header-line nil :background nil :inherit 'default)

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                term-mode-hook
                shell-mode-hook
                xwidget-webkit-mode-hook
                mu4e-main-mode-hook
                mu4e-view-mode-hook
                mu4e-headers-mode-hook
                deft-mode-hook
                pdf-view-mode-hook
                help-mode-hook
                image-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'prog-mode-hook 'hl-line-mode)

;; WIP
;; (advice-add 'counsel-describe-face :before '(lambda () (hl-line-mode 0))) ;; This works
;; (advice-add 'describe-face :after '(lambda () (hl-line-mode 1))) ;; This doesn't

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'hide-mode-line-mode)
  :config
  (setq dashboard-items '(
                          ;; (bookmarks  . 5)
                          ;; (recents . 6)
                          (registers . 14)))

  ;; Header, footer, messages
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-footer-messages '(""))
  (setq dashboard-footer-icon (all-the-icons-octicon "zap"
                                                     :height 0.00001
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))
  (setq dashboard-startup-banner 'logo)
  ;; (setq dashboard-startup-banner '"~/Dropbox/Mackup/emacs-stuff/banner.txt")
  ;; General config
  (setq dashboard-center-content t
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-week-agenda nil
        dashboard-center-content t
        dashboard-set-init-info nil
        dashboard-set-navigator t
        dashboard-items-default-length 30
        dashboard-page-separator "\n\n")
  (dashboard-setup-startup-hook)
  :general
  (general-define-key :keymaps 'dashboard-mode-map
                      "e" nil))

(use-package visual-fill-column
  :defer t
  :config
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t))

(use-package writeroom-mode
  :defer t
  :config
  (setq writeroom-maximize-window nil
        writeroom-header-line "" ;; Makes sure we have a header line, that's blank
        writeroom-mode-line t
        writeroom-global-effects nil) ;; No need to have Writeroom do any of that silly stuff
  (setq writeroom-width 70)
  ;; (add-hook 'writeroom-mode-hook (lambda () (setq-local line-spacing 10)))
  )

(use-package centered-cursor-mode
  :diminish centered-cursor-mode)

(defun jib/pulse-area (&rest _)
  "Pulse +-5 chars of point."
  (pulse-momentary-highlight-region (- (point) 5) (+ 5 (point))))

(dolist (command '(org-forward-sentence org-backward-sentence))
  (advice-add command :after #'pulse-area))

;; WIP STUFF

;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

;; (defun pulse-line (&rest _)
;;   "Pulse the current line."
;;   (interactive)
;;   (pulse-momentary-highlight-one-line (point)))

;; (dolist (command '(scroll-up-command scroll-down-command
;;                                      recenter-top-bottom other-window select-window-by-number))
;;   (advice-add command :after #'pulse-line))
;; (defadvice other-window (after other-window-pulse activate) (pulse-line))
;; (defadvice delete-window (after delete-window-pulse activate) (pulse-line))
;; (defadvice recenter-top-bottom (after recenter-top-bottom-pulse activate))

;; (defun pulse-line (&rest _)
;;       "Pulse the current line."
;;       (pulse-momentary-highlight-one-line (point)))
;; (pulse-momentary-highlight

;; (dolist (command '(scroll-up-command scroll-down-command
;;                    recenter-top-bottom other-window evil-window-next))
;;   (advice-add command :after #'pulse-line))

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil) ;; takes over 'j'
  (setq org-super-agenda-header-prefix " ◦ ") ;; There are some unicode "THIN SPACE"s after the ◦
  (org-super-agenda-mode))

(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("INPROG-TODO" . 9744)
                                          ("HW" . 9744)
                                          ("STUDY" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("READ" . 9744)
                                          ("PROJ" . 9744)
                                          ("CONTACT" . 9744)
                                          ("DONE" . 9745)))
  :hook (org-mode . org-superstar-mode))

;; Removes gap when you add a new heading
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme))))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(use-package org-gcal
  :defer t
  :config
  (setq org-gcal-down-days '20) ;; Only fetch events 20 days into the future
  (setq org-gcal-up-days '10) ;; Only fetch events 10 days into the past
  (setq org-gcal-recurring-events-mode 'top-level)
  (setq org-gcal-remove-api-cancelled-events t) ;; No prompt when deleting removed events

  ;; NOTE - org-gcal ids and calendar configuation is set in 'private.el' for sake of security/privacy.
  )

(use-package org-appear
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t) ;; A default setting that needs to be t for org-appear

  (setq org-appear-autoemphasis t)  ;; Enable org-appear on emphasis (bold, italics, etc)
  (setq org-appear-autolinks t) ;; Enable on links
  (setq org-appear-autosubmarkers t)) ;; Enable on subscript and superscript

(use-package ox-reveal
  :defer 5)

(setq org-modules '(org-habit))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package org-ql
  :general
  (general-define-key :keymaps 'org-ql-view-map
                      "q" 'kill-buffer-and-window)
  )

;; (use-package org-pretty-tags
;;   :config
;;   (setq org-pretty-tags-surrogate-strings
;;         (quote
;;          (("bv" . "")
;;           ("sp" . "")
;;           ("security" . "🔥"))))
;;   (org-pretty-tags-global-mode))

;; Org-agenda specific bindings
(evil-define-key 'motion org-agenda-mode-map
  (kbd "f") 'org-agenda-later
  (kbd "b") 'org-agenda-earlier)

(defun jib/org-font-setup ()
  (set-face-attribute 'org-document-title nil :height 1.1) ;; Bigger titles, smaller drawers
  (set-face-attribute 'org-checkbox-statistics-done nil :inherit 'org-done :foreground "green3") ;; Makes org done checkboxes green
  ;; (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch :inherit 'shadow :height 0.6 :foreground nil) ;; Makes org-drawer way smaller
  (set-face-attribute 'org-ellipsis nil :inherit 'shadow :height 0.8) ;; Makes org-ellipsis shadow (blends in better)
  (set-face-attribute 'org-scheduled-today nil :weight 'normal) ;; Removes bold from org-scheduled-today
  (set-face-attribute 'org-super-agenda-header nil :inherit 'org-agenda-structure :weight 'bold) ;; Bolds org-super-agenda headers
  (set-face-attribute 'org-scheduled-previously nil :background "red") ;; Bolds org-super-agenda headers

  ;; Here I set things that need it to be fixed-pitch, just in case the font I am using isn't monospace.
  ;; (dolist (face '(org-list-dt org-tag org-todo org-table org-checkbox org-priority org-date org-verbatim org-special-keyword))
  ;;   (set-face-attribute `,face nil :inherit 'fixed-pitch))

  (dolist (face '(org-code org-verbatim org-ellipsis org-meta-line))
    (set-face-attribute `,face nil :inherit 'shadow :inherit 'fixed-pitch))
  )

(defun jib/prettify-symbols-setup ()
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  ;; (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[X]" . "☒" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)

  (push '(":Misc:" . "" ) prettify-symbols-alist)
  (push '(":ec:" . "" ) prettify-symbols-alist)
  (push '(":Weekly:ec:" . "" ) prettify-symbols-alist)
  (push '(":Robo:ec:" . "" ) prettify-symbols-alist)

  (push '(":bv:" . "" ) prettify-symbols-alist)
  (push '(":sp:" . "") prettify-symbols-alist)
  (push '(":cl:" . "𝛑" ) prettify-symbols-alist)
  (push '(":ch:" . "" ) prettify-symbols-alist)
  (push '(":es:" . "" ) prettify-symbols-alist)
  (prettify-symbols-mode)

  ;; (defvar svg-font-lock-keyword  
  ;;   `(("TODO"
  ;;      (0 (list
  ;;          'face nil
  ;;          'display (svg-lib-tag "TODO" nil :stroke 2 :font-family "Roboto Mono" :font-weight 500 :padding 1 :foreground "plum3" :radius 5))))))

  ;; ;; activate
  ;; (push 'display font-lock-extra-managed-props)
  ;; (font-lock-add-keywords nil svg-font-lock-keyword)
  ;; (font-lock-flush (point-min) (point-max))
  )

(defun jib/org-setup ()
  (org-indent-mode) ;; Keeps org items like text under headings, lists, nicely indented
  (visual-line-mode 1) ;; Nice line wrapping

  (centered-cursor-mode)

  ;; (setq header-line-format "") ;; Empty header line, basically adds a blank line on top
  (setq-local line-spacing (+ jib-default-line-spacing 1))
  )

(use-package org
  :pin gnu
  :hook (org-mode . jib/org-setup)
  :hook (org-mode . jib/org-font-setup)
  :hook (org-mode . jib/prettify-symbols-setup)
  :hook (org-capture-mode . evil-insert-state) ;; Start org-capture in Insert state by default
  :diminish org-indent-mode
  :diminish visual-line-mode
  :config

;; (setq org-ellipsis "⤵")
(setq org-ellipsis " ▼ ")
(setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
(setq org-startup-folded t) ;; Org files start up folded by default
(setq org-image-actual-width nil)

(setq org-cycle-separator-lines 1)
(setq org-catch-invisible-edits 'smart)
(setq org-src-tab-acts-natively t)

;; M-Ret can split lines on items and tables but not headlines and not on anything else (unconfigured)
(setq org-M-RET-may-split-line '((headline) (item . t) (table . t) (default)))
(setq org-loop-over-headlines-in-active-region nil)

;; Opens links to other org file in same frame (rather than splitting)
(setq org-link-frame-setup '((file . find-file)))

(setq org-log-done t)
(setq org-log-into-drawer t)

;; Automatically change bullet type when indenting
;; Ex: indenting a + makes the bullet a *.
(setq org-list-demote-modify-bullet
      '(("+" . "*") ("*" . "-") ("-" . "+")))

;; Automatically save and close the org files I most frequently archive to.
;; I see no need to keep them open and crowding my buffer list.
;; Uses my own function jib/save-and-close-this-buffer.
(dolist (file '("homework-archive.org_archive" "todo-archive.org_archive"))
  (advice-add 'org-archive-subtree-default :after 
              (lambda () (jib/save-and-close-this-buffer file))))

(setq counsel-org-tags '("qp" "ec" "st")) ;; Quick-picks, extracurricular, short-term

(setq org-tag-faces '(
                      ("bv" . "dark slate blue")
                      ("sp" . "purple3")
                      ("ch" . "PaleTurquoise3")
                      ("cl" . "chartreuse4")
                      ("es" . "brown3")
                      ("Weekly" . "SteelBlue1")
                      ("Robo" . "IndianRed2")
                      ("Misc" . "tan1")
                      ("qp" . "RosyBrown1") ;; Quick-picks
                      ("ec" . "PaleGreen3") ;; Extracurricular
                      ("st" . "DimGrey") ;; Near-future (aka short term) todo
                      ))

;; (setq org-tags-column -64)
(setq org-tags-column 1)

(setq org-todo-keywords '((type
                           "TODO(t)" "INPROG-TODO(i)" "HW(h)" "STUDY" "SOMEDAY"
                           "READ(r)" "PROJ(p)" "CONTACT(c)"
                           "|" "DONE(d)" "CANCELLED(C)")))

(setq org-todo-keyword-faces '(("TODO" nil :foreground "orange1" :inherit fixed-pitch :weight medium)
                               ("HW" nil :foreground "coral1" :inherit fixed-pitch :weight medium)
                               ("STUDY" nil :foreground "plum3" :inherit fixed-pitch :weight medium)
                               ("SOMEDAY" nil :foreground "steel blue" :inherit fixed-pitch)
                               ("CONTACT" nil :foreground "LightSalmon2" :inherit fixed-pitch :weight medium)
                               ("READ" nil :foreground "MediumPurple3" :inherit fixed-pitch :weight medium)
                               ("PROJ" nil :foreground "aquamarine3" :inherit fixed-pitch :weight medium)

                               ("INPROG-TODO" nil :foreground "orange1" :inherit fixed-pitch :weight medium)

                               ("DONE" nil :foreground "LawnGreen" :inherit fixed-pitch :weight medium)
                               ("CANCELLED" nil :foreground "dark red" :inherit fixed-pitch :weight medium)))

(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#D].

(setq org-priority-faces
'((65 nil :inherit fixed-pitch :foreground "red2" :weight medium)
  (66 . "Gold1")
  (67 . "Goldenrod2")
  (68 . "PaleTurquoise3")
  (69 . "DarkSlateGray4")
  (70 . "PaleTurquoise4")))

;; Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   (gnuplot . t)
   ))

(use-package gnuplot)

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
(setq python-shell-completion-native-enable nil)

;; How to open buffer when calling `org-edit-special'.
(setq org-src-window-setup 'current-window)

(setq org-habit-preceding-days 3)
(setq org-habit-following-days 3)
;; (setq org-habit-today-glyph ?X);;‖

(setq org-habit-graph-column 40)

;; Uses custom time stamps
(setq org-time-stamp-custom-formats '("<%A, %B %d, %Y" . "<%m/%d/%y %a %I:%M %p>"))

(setq org-agenda-restore-windows-after-quit t)

;; Only show upcoming deadlines for tomorrow or the day after tomorrow. By default it shows
;; 14 days into the future, which seems excessive.
(setq org-deadline-warning-days 2)
;; If something is done, don't show it's deadline
(setq org-agenda-skip-deadline-if-done t)
;; If something is done, don't show when it's scheduled for
(setq org-agenda-skip-scheduled-if-done t)
;; If something is scheduled, don't tell me it is due soon
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)


(setq org-agenda-timegrid-use-ampm 1)

;; (setq org-agenda-time-grid '((daily today require-timed)
;;                              (800 900 1000 1100 1200 1300 1400 1500 1600 1700)
;;                              "        "
;; 							 "----------------"))

(setq org-agenda-time-grid nil) ;; I've decided to disable the time grid. 2021-09-22.

(setq org-agenda-block-separator 8213) ;; Unicode: ―
(setq org-agenda-current-time-string "<----------------- Now")
(setq org-agenda-scheduled-leaders '("" ""))

(setq org-agenda-prefix-format '((agenda . " %i %-1:i%?-2t% s")
                                 (todo . "   ")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

;; https://stackoverflow.com/questions/58820073/s-in-org-agenda-prefix-format-doesnt-display-dates-in-the-todo-view
;; something to look into

(setq org-agenda-custom-commands
      '(
        ("n" "Super zaen view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '(
                         (:name "Schedule"
                                :time-grid t
                                ;; :discard (:anything t)
                                ;; :discard (:scheduled today)
                                :order 1)
                         (:name "Today's Tasks"
                                ;; Still working on this. Not how I want yet.
                                ;; :discard (:not (:scheduled today))
                                ;; :discard (:deadline today)
                                :scheduled t
                                :order 2)
                         (:name "Unscheduled Deadlines"
                                :deadline t
                                :order 3)
                         ))))

          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          (:name "Overdue"
                                 :face (:background "red")
                                 :scheduled past
                                 :deadline past
                                 :order 2)
                          (:name "Important"
                                 :discard (:tag "habit")
                                 :and (:todo "TODO" :priority "A") ;; Homework doesn't count here
                                 :todo "CONTACT"
                                 :order 3)
                          (:name "Short-term Todo"
                                 :tag "st"
                                 :order 4)
                          (:name "Personal"
                                 :category "personal"
                                 :order 40)
                          (:name "Someday"
                                 :todo "SOMEDAY"
                                 :order 30)
                          (:name "Homework"
                                 :todo ("HW" "READ")
                                 :order 5)
                          (:name "Studying"
                                 :todo "STUDY"
                                 :order 7)
                          (:name "Quick Picks"
                                 :tag "qp"
                                 :order 11)
                          (:name "College"
                                 :category "college"
                                 :order 35)
                          (:name "Projects"
                                 :todo "PROJ"
                                 :order 12)
                          (:name "Weekly"
                                 :tag "weekly"
                                 :order 15)
                          (:name "Extracurricular"
                                 :discard (:todo "SOMEDAY")
                                 :tag ("robotics" "ec")
                                 :order 13)
                          (:name "Todo"
                                 :discard (:category "personal")
                                 :todo ("TODO" "INPROGRESS-TODO")
                                 :order 20)
                          ))))))
        ("m" "Agendaless Super zaen view"
         (            (alltodo "" ((org-agenda-overriding-header "Agendaless Todo View")
                                   (org-super-agenda-groups
                                    '(
                                      (:name "Today's Tasks"
                                             :scheduled today
                                             :deadline today)
                                      (:name "Overdue"
                                             :face (:background "red")
                                             :scheduled past
                                             :discard (:tag "habit")
                                             :deadline past
                                             :order 2)
                                      (:name "Important"
                                             :and (:todo "TODO" :priority "A") ;; Homework doesn't count here
                                             :todo "CONTACT"
                                             :order 3)
                                      (:name "Short-term Todo"
                                             :tag "st"
                                             :order 4)
                                      (:name "Personal"
                                             :category "personal"
                                             :order 40)
                                      (:name "Someday"
                                             :todo "SOMEDAY"
                                             :order 30)
                                      (:name "Homework"
                                             :todo ("HW" "READ")
                                             :order 5)
                                      (:name "Studying"
                                             :todo "STUDY"
                                             :order 7)
                                      (:name "Quick Picks"
                                             :tag "qp"
                                             :order 11)
                                      (:name "College"
                                             :category "college"
                                             :order 35)
                                      (:name "Projects"
                                             :todo "PROJ"
                                             :order 12)
                                      (:name "Weekly"
                                             :tag "weekly"
                                             :order 15)
                                      (:name "Extracurricular"
                                             :discard (:todo "SOMEDAY")
                                             :tag ("robotics" "ec")
                                             :order 20)
                                      (:name "Todo"
                                             :discard (:tag "habit")
                                             :discard (:category "personal")
                                             :todo ("TODO" "INPROGRESS-TODO")
                                             :order 13)
                                      ))))))

        ("p" "Agendaless w/o scheduled Super zaen view"
         (            (alltodo "" ((org-agenda-overriding-header "Agendaless Todo View - Today and Unscheduled Tasks")
                                   (org-super-agenda-groups
                                    '(
                                      (:name "Today's Tasks"
                                             :scheduled today
                                             :discard (:scheduled future)
                                             :deadline today)
                                      (:name "Overdue"
                                             :face (:background "red")
                                             :scheduled past
                                             :discard (:tag "habit")
                                             :deadline past
                                             :order 2)
                                      (:name "Important"
                                             :and (:todo "TODO" :priority "A") ;; Homework doesn't count here
                                             :todo "CONTACT"
                                             :order 3)
                                      (:name "Short-term Todo"
                                             :tag "st"
                                             :order 4)
                                      (:name "Someday"
                                             :todo "SOMEDAY"
                                             :order 30)
                                      (:name "Homework"
                                             :todo ("HW" "READ")
                                             :order 5)
                                      (:name "Studying"
                                             :todo "STUDY"
                                             :order 7)
                                      (:name "Quick Picks"
                                             :tag "qp"
                                             :order 11)
                                      (:name "College"
                                             :category "college"
                                             :order 35)
                                      (:name "Projects"
                                             :todo "PROJ"
                                             :order 12)
                                      (:name "Weekly"
                                             :tag "weekly"
                                             :order 15)
                                      (:name "Extracurricular"
                                             :discard (:todo "SOMEDAY")
                                             :tag ("robotics" "ec")
                                             :order 20)
                                      (:name "Personal"
                                             :category "personal"
                                             :order 27)
                                      (:name "Todo"
                                             :discard (:tag "habit")
                                             :discard (:category "personal")
                                             :todo ("TODO" "INPROGRESS-TODO")
                                             :order 13)
                                      ))))))


        ))
;; Org-super-agenda-mode itself is activated in the use-package block

;; By default an org-capture/refile will save a bookmark. This
;; disables that and keeps my bookmark list how I want it.
(setq org-bookmark-names-plist nil)

(setq org-refile-targets (quote (("~/Dropbox/org/work.org" :maxlevel . 2))))
(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t)          ; Show full paths for refilin0


(setq org-capture-templates
      '(
        ("n" "CPB Note" entry (file+headline "~/Dropbox/org/cpb.org" "Refile")
         "** Note: %? @ %U" :empty-lines 0)

        ("w" "Work Todo Entries")
            ("we" "No Time" entry (file "~/Dropbox/org/work.org")
             "** %^{Type|TODO|HW|READ|PROJ} %^{Todo title} %?" :prepend t :empty-lines-before 0)

            ("ws" "Scheduled" entry (file "~/Dropbox/org/work.org")
             "** %^{Type|TODO|HW|READ|PROJ} %^{Todo title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0)

            ("wd" "Deadline" entry (file "~/Dropbox/org/work.org")
             "** %^{Type|TODO|HW|READ|PROJ} %^{Todo title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0)

            ("ww" "Scheduled & deadline" entry (file "~/Dropbox/org/work.org")
             "** %^{Type|TODO|HW|READ|PROJ} %^{Todo title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0)
        ))

(setq org-export-backends '(ascii beamer html latex md odt))

(setq org-export-with-broken-links t)
(setq org-export-with-smart-quotes t)
(setq org-export-allow-bind-keywords t)

;; From https://stackoverflow.com/questions/23297422/org-mode-timestamp-format-when-exported
(defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
  "removes relevant brackets from a timestamp"
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))


;; HTML-specific
(setq org-html-validation-link nil) ;; No validation button on HTML exports

;; LaTeX Specific
(eval-after-load 'ox '(add-to-list
                       'org-export-filter-timestamp-functions
                       'org-export-filter-timestamp-remove-brackets))

(use-package ox-hugo
  :defer 2
  :after ox
  :config
  (setq org-hugo-base-dir "~/Dropbox/Projects/cpb"))

(setq org-latex-listings t) ;; Uses listings package for code exports
(setq org-latex-compiler "xelatex") ;; XeLaTex rather than pdflatex

;; not sure what this is, look into it
;; '(org-latex-active-timestamp-format "\\texttt{%s}")
;; '(org-latex-inactive-timestamp-format "\\texttt{%s}")

;; LaTeX Classes
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex" ;; I use this in base class in all of my org exports.
                 "\\documentclass{extarticle}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(setq org-clock-mode-line-total 'current) ;; Show only timer from current clock session in modeline

(setq org-attach-id-dir ".org-attach/")


) ;; This parenthesis ends the org use-package.

(defun jib/deft-kill ()
  (kill-buffer "*Deft*"))

(defun jib/deft-evil-fix ()
  (evil-insert-state)
  (centered-cursor-mode))

(use-package deft
  :config
  (setq deft-directory (concat jib/dropbox "notes/")
        deft-extensions '("org" "txt")
        deft-recursive t
        deft-file-limit 40
        deft-use-filename-as-title t)

  (add-hook 'deft-open-file-hook 'jib/deft-kill) ;; Once a file is opened, kill Deft
  (add-hook 'deft-mode-hook 'jib/deft-evil-fix) ;; Goes into insert mode automaticlly in Deft

  ;; Removes :PROPERTIES: from descriptions
  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  :general

  (general-define-key :states 'normal :keymaps 'deft-mode-map
                      ;; 'q' kills Deft in normal mode
                      "q" 'kill-this-buffer)

  (general-define-key :states 'insert :keymaps 'deft-mode-map
                      "C-j" 'next-line
                      "C-k" 'previous-line)
  )

(use-package latex ;; This is a weird one. Package is auctex but needs to be managed like this.
  :ensure nil
  :defer t
  :init
  (setq TeX-engine 'xetex ;; Use XeTeX
        latex-run-command "xetex")

  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        ;; Use directories in a hidden away folder for AUCTeX files.
        TeX-auto-local (concat user-emacs-directory "auctex/auto/")
        TeX-style-local (concat user-emacs-directory "auctex/style/")

        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex

        TeX-show-compilation nil

        ;; Don't start the Emacs server when correlating sources.
        TeX-source-correlate-start-server nil

        ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Just save, don't ask before each compilation.
        TeX-save-query nil)

  ;; To use pdfview with auctex:
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)
  :general
  (general-define-key
    :prefix ","
    :states 'normal
    :keymaps 'LaTeX-mode-map
    "" nil
    "a" '(TeX-command-run-all :which-key "TeX run all")
    "c" '(TeX-command-master :which-key "TeX-command-master")
    "c" '(TeX-command-master :which-key "TeX-command-master")
    "e" '(LaTeX-environment :which-key "Insert environment")
    "s" '(LaTeX-section :which-key "Insert section")
    "m" '(TeX-insert-macro :which-key "Insert macro")
    )

  )

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; Standard way

(use-package company-auctex
  :after auctex
  :init
  (add-to-list 'company-backends 'company-auctex)
  (company-auctex-init))

(use-package mw-thesaurus
  :defer t
  :config
  ;; Binds q to quit in mw-thesaurus
  (add-hook 'mw-thesaurus-mode-hook (lambda () (define-key evil-normal-state-local-map (kbd "q") 'mw-thesaurus--quit))))

(use-package pdf-tools
  :defer t
  :pin manual
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-height)
  (setq pdf-view-continuous nil) ;; Makes it so scrolling down to the bottom/top of a page doesn't switch to the next page
  (setq pdf-view-midnight-colors '("#ffffff" . "#121212" )) ;; I use midnight mode as dark mode, dark mode doesn't seem to work
  :general
  (general-define-key :states 'motion :keymaps 'pdf-view-mode-map
                      "j" 'pdf-view-next-page
                      "k" 'pdf-view-previous-page

                      "C-j" 'pdf-view-next-line-or-next-page
                      "C-k" 'pdf-view-previous-line-or-previous-page

                      ;; Arrows for movement as well
                      (kbd "<down>") 'pdf-view-next-line-or-next-page
                      (kbd "<up>") 'pdf-view-previous-line-or-previous-page

                      (kbd "<down>") 'pdf-view-next-line-or-next-page
                      (kbd "<up>") 'pdf-view-previous-line-or-previous-page

                      (kbd "<left>") 'image-backward-hscroll
                      (kbd "<right>") 'image-forward-hscroll

                      "H" 'pdf-view-fit-height-to-window
                      "0" 'pdf-view-fit-height-to-window
                      "W" 'pdf-view-fit-width-to-window
                      "=" 'pdf-view-enlarge
                      "-" 'pdf-view-shrink

                      "q" 'quit-window
                      "Q" 'kill-this-buffer
                      "g" 'revert-buffer
                      )
  )

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Warnings\\*"
          help-mode
          compilation-mode))
  (popper-mode +1))

(use-package projectile
  :defer t
  :general
  (:keymaps 'projectile-mode-map
            "s-f" 'projectile-find-file
            "s-p" 'projectile-command-map
            "C-c p" 'projectile-command-map
            "s-c" 'projectile-commander)
  :init
  (projectile-mode +1))

(use-package rainbow-mode
  :defer t)


(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF4500")
        ("DEBUG"  . "#A020F0")
        ("WIP"   . "#1E90FF"))))

;; A better python mode (supposedly)
(use-package python-mode
  :defer t)

;; Using my virtual environments
(use-package pyvenv
  :defer t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")) ;; Where the virtual envs are stored on my computer


;; Automatically set the virtual environment when entering a directory
(use-package auto-virtualenv
  :defer 2
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

;; Python development helper
;; (use-package elpy
;;   :defer t
;;   :init
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   (advice-add 'python-mode :before 'elpy-enable))

(use-package web-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)) ;; Open .html files in web-mode
  :config
  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)

  :general
  (general-def
  :prefix ","
  :states 'motion
  :keymaps 'web-mode-map
  "" nil
  "i" '(web-mode-buffer-indent :which-key "web mode indent")
  "c" '(web-mode-fold-or-unfold :which-key "web mode toggle fold")
  ))
