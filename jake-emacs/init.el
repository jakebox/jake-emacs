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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) ;; ELPA and NonGNU ELPA are default in Emacs28

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; w/o this Emacs freezes when refreshing ELPA

(package-initialize)
(setq package-enable-at-startup nil)

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

(load (expand-file-name "jib-variables.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculated variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set `jib/computer' to 'laptop or 'desktop.
(let ((sys (system-name)))
  (if (or (string= sys "MJBs-MacBook-Air.local") (string= sys "mjbs-air.lan"))
      (setq jib/computer 'laptop)
    (setq jib/computer 'desktop)))

(let ((sys (system-name)))
  (if (string= sys "KilgoreTrout.local")
      (setq jib/computer 'laptopN)))

(setq jib/computer 'laptopN) ;; manual override 2022-09-14

(load (expand-file-name "jib-funcs.el" user-emacs-directory))
(load (expand-file-name "private.el" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load custom-file)

;; I put mostly stuff I find online in this "lisp" folder in my emacs-stuff.
;; Add every directory in that folder to the load-path.
(let ((default-directory (directory-file-name (concat jib/emacs-stuff "/lisp"))))
  (normal-top-level-add-subdirs-to-load-path))

(require 'on) ;; on.el – utility hooks and functions from Doom Emacs

(setq register-preview-delay 0) ;; Show registers ASAP

(set-register ?i (cons 'file (concat org-directory   "/cpb.org")))
(set-register ?h (cons 'file (concat org-directory   "/work.org")))
(set-register ?C (cons 'file (concat jib/emacs-stuff "/jake-emacs/init.org")))
(set-register ?A (cons 'file (concat org-directory   "/org-archive/homework-archive.org_archive")))
(set-register ?T (cons 'file (concat org-directory   "/org-archive/todo-archive.org_archive")))

;; (if (eq jib/computer 'laptopN)
;;     (progn
;;       (setenv "PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/jake/.pyenv/bin:/usr/local/sbin:/Users/jake/bin:/Users/jake/.doom-emacs/bin:/Library/TeX/texbin")
;;       ;; (setq exec-path '("/opt/homebrew/bin" "/opt/homebrew/sbin" "/usr/local/bin" "/usr/bin" "/usr/sbin" "/bin"))
;;       )
;;   (progn
;;     ;; (setenv "PATH" "/usr/local/Cellar/pyenv-virtualenv/1.1.5/shims:/Users/jake/.pyenv/shims:/usr/local/bin:/bin:/usr/bin:/usr/sbin:/usr/local/sbin:/sbin:/Users/jake/bin:/Users/jake/doom-emacs/bin:/Library/TeX/texbin")
;;     (setq exec-path '("/usr/local/Cellar/pyenv-virtualenv/1.1.5/shims"
;;                       "/Users/jake/.pyenv/shims" "/usr/local/bin" "/bin"
;;                       "/usr/bin" "/usr/sbin" "/usr/local/sbin" "/sbin"
;;                       "/Users/jake/bin" "/Users/jake/doom-emacs/bin"
;;                       "/Library/TeX/texbin"))))

(if (eq jib/computer 'laptopN)
    (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq insert-directory-program "/usr/local/bin/gls"))

(setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
(setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

(server-start)

;; A cool mode to revert window configurations.
(winner-mode 1)

;; INTERACTION -----
(setq use-short-answers t) ;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(setq confirm-kill-emacs 'yes-or-no-p) ;; Confirm to quit
(setq initial-major-mode 'org-mode ;; Major mode of new buffers
      initial-scratch-message ""
      initial-buffer-choice t) ;; Blank scratch buffer

;; WINDOW -----------
(setq frame-resize-pixelwise t)
(setq ns-pop-up-frames nil) ;; When opening a file (like double click) on Mac, use an existing frame
(setq window-resize-pixelwise nil)
(setq split-width-threshold 80) ;; How thin the window should be to stop splitting vertically (I think)

;; LINES -----------
(setq-default truncate-lines t)
(setq-default tab-width 4)
(setq-default fill-column 80)
(setq line-move-visual t) ;; C-p, C-n, etc uses visual lines

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

(setq bookmark-set-fringe-mark nil)

;; SCROLLING ---------
(setq scroll-conservatively 101)
(setq
 mouse-wheel-follow-mouse 't
 mouse-wheel-progressive-speed nil
 ;; The most important setting of all! Make each scroll-event move 2 lines at
 ;; a time (instead of 5 at default). Simply hold down shift to move twice as
 ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
 mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 6)))
(setq mac-redisplay-dont-reset-vscroll t ;; sane trackpad/mouse scroll settings (doom)
      mac-mouse-wheel-smooth-scroll nil)

;; BELL/WARNING ------------
(setq visible-bell nil) ;; Make it ring (so no visible bell) (default)
(setq ring-bell-function 'ignore) ;; BUT ignore it, so we see and hear nothing


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
  (setq recentf-filename-handlers ;; Show home folder path as a ~
        (append '(abbreviate-file-name) recentf-filename-handlers))
  (recentf-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ENCODING -------------
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

(setq default-input-method "spanish-postfix") ;; When I need to type in Spanish (switch with C-\)

(setq blink-cursor-interval 0.6)
(blink-cursor-mode 0)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(setq what-cursor-show-names t) ;; improves C-x =

;; Weird thing where `list-colors-display` doesn't show all colors.
;; https://bug-gnu-emacs.gnu.narkive.com/Bo6OdySs/bug-5683-23-1-93-list-colors-display-doesn-t-show-all-colors
(setq x-colors (ns-list-colors))

(setq dired-kill-when-opening-new-dired-buffer t)

(setq reb-re-syntax 'string) ;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-prefix-prefix "◉ ")
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 3
        which-key-max-display-columns nil))

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; don't load Evil keybindings in other modes
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)
  :config

  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'bufler-list-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'calc-mode 'emacs)

  ;; ----- Keybindings
  ;; I tried using evil-define-key for these. Didn't work.
  (define-key evil-window-map "\C-q" 'evil-delete-buffer) ;; Maps C-w C-q to evil-delete-buffer (The first C-w puts you into evil-window-map)
  (define-key evil-window-map "\C-w" 'kill-this-buffer)
  (define-key evil-motion-state-map "\C-b" 'evil-scroll-up) ;; Makes C-b how C-u is
  (define-key evil-motion-state-map [?\s-\\] 'evil-execute-in-emacs-state) ;; `super-\', by default it's just `\'

  ;; ----- Setting cursor colors
  (setq evil-emacs-state-cursor    '("#649bce" box))
  (setq evil-normal-state-cursor   '("#d9a871" box))
  (setq evil-operator-state-cursor '("#ebcb8b" hollow))
  (setq evil-visual-state-cursor   '("#677691" box))
  (setq evil-insert-state-cursor   '("#eb998b" (bar . 2)))
  (setq evil-replace-state-cursor  '("#eb998b" hbar))
  (setq evil-motion-state-cursor   '("#ad8beb" box))

  (evil-mode 1))

(use-package evil-surround
  :after evil
  :defer 2
  :config
  (global-evil-surround-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (setq evil-collection-mode-list '(dired (custom cus-edit) (package-menu package) calc diff-mode))
;;   (evil-collection-init))

;; (use-package evil-snipe
;;   :diminish evil-snipe-mode
;;   :diminish evil-snipe-local-mode
;;   :after evil
;;   :config
;;   (evil-snipe-mode +1))

;; not working right now, from https://jblevins.org/log/dired-open
;; (evil-define-key 'motion 'dired-mode-map "s-o" '(lambda () (interactive)
;; 												  (let ((fn (dired-get-file-for-visit)))
;; 													(start-process "default-app" nil "open" fn))))

;;  (evil-define-key 'motion 'dired-mode-map "Q" 'kill-this-buffer)
(evil-define-key 'motion help-mode-map "q" 'kill-this-buffer)
(evil-define-key 'motion calendar-mode-map "q" 'kill-this-buffer)

(use-package general)

(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"

 ;; Top level functions
 "/" '(jib/rg :which-key "ripgrep")
 ";" '(spacemacs/deft :which-key "deft")
 ":" '(project-find-file :which-key "p-find file")
 "." '(counsel-find-file :which-key "find file")
 "," '(counsel-recentf :which-key "recent files")
 "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
 "SPC" '(counsel-find-file :which-key "M-x")
 "q" '(save-buffers-kill-terminal :which-key "quit emacs")
 "r" '(jump-to-register :which-key "registers")
 "c" 'org-capture

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
"abg" '(jib/er-google :which-key "google search")

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
"jb" '((lambda() (interactive) (find-file (concat jib/dropbox "org/work.org"))) :which-key "work.org")
"jc" '((lambda() (interactive) (find-file (concat jib/dropbox "org/cpb.org"))) :which-key "cpb.org")

"jr" '(restart-emacs :which-key "restart emacs")

"jh" '(nil :which-key "hydras")
"jht" '(jb-hydra-theme-switcher/body :which-key "themes")
"jhf" '(jb-hydra-variable-fonts/body :which-key "mixed-pitch face")
"jhw" '(jb-hydra-window/body :which-key "window control")

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

"hed" '((lambda () (interactive) (jump-to-register 67)) :which-key "edit dotfile")

"hm" '(nil :which-key "switch mode")
"hme" '(emacs-lisp-mode :which-key "elisp mode")
"hmo" '(org-mode :which-key "org mode")
"hmt" '(text-mode :which-key "text mode")

"hp" '(nil :which-key "packages")
"hpr" 'package-refresh-contents
"hpi" 'package-install
"hpd" 'package-delete

;; Help/emacs
"x" '(nil :which-key "text")
"xC" '(jib/copy-whole-buffer-to-clipboard :which-key "copy whole buffer to clipboard")
"xr" '(anzu-query-replace :which-key "find and replace")
"xs" '(yas-insert-snippet :which-key "insert yasnippet")
"xf" '(flush-lines :which-key "flush-lines")
"xR" '(replace-regexp :which-key "replace-regexp")

;; Toggles
"t" '(nil :which-key "toggles")
"tt" '(toggle-truncate-lines :which-key "truncate lines")
"tv" '(visual-line-mode :which-key "visual line mode")
"tn" '(display-line-numbers-mode :which-key "display line numbers")
"ta" '(mixed-pitch-mode :which-key "variable pitch mode")
"ty" '(counsel-load-theme :which-key "load theme")
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
"wc" '(evil-window-delete :which-key "delete window")
"w-" '(jib/split-window-vertically-and-switch :which-key "split below")
"w/" '(jib/split-window-horizontally-and-switch :which-key "split right")
"wr" '(jb-hydra-window/body :which-key "hydra window")
"wl" '(evil-window-right :which-key "evil-window-right")
"wh" '(evil-window-left :which-key "evil-window-left")
"wj" '(evil-window-down :which-key "evil-window-down")
"wk" '(evil-window-up :which-key "evil-window-up")
"wz" '(text-scale-adjust :which-key "text zoom")
) ;; End SPC prefix block

;; All-mode keymaps
(general-def
  :keymaps 'override

  ;; Emacs --------
  "M-x" 'counsel-M-x
  "ß" 'evil-window-next ;; option-s
  "Í" 'other-frame ;; option-shift-s
  "C-S-B" 'counsel-switch-buffer
  "∫" 'counsel-switch-buffer ;; option-b
  "s-b" 'counsel-switch-buffer ;; super-b
  "s-o" 'jb-hydra-window/body

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

  ;; Utility ------
  "C-c c" 'org-capture
  "C-c a" 'org-agenda
  "C-s" 'swiper ;; Large files will use grep (faster)
  "s-\"" 'ispell-word ;; that's super-shift-'
  "M-+" 'jib/calc-speaking-time
  "M-=" 'count-words
  "C-'" 'avy-goto-char-2
  "C-x C-b" 'bufler-list

  ;; super-number functions
  "s-1" 'mw-thesaurus-lookup-dwim
  "s-!" 'mw-thesaurus-lookup
  "s-2" 'ispell-buffer
  "s-3" 'revert-buffer
  "s-4" '(lambda () (interactive) (counsel-file-jump nil jib/dropbox))
  "s-5" '(lambda () (interactive) (counsel-rg nil jib/dropbox))
  "s-6" 'org-capture
  "s-7" 'jib/open-dropbox-folder-in-finder
  "s-8" 'jib/zoxide-wrapper

  "s-w" 'kill-this-buffer
  )

;; Non-insert mode keymaps
(general-def
  :states '(normal visual motion)
  "gc" 'comment-dwim
  "gC" 'comment-line

  "u" 'undo-fu-only-undo
  "U" 'undo-fu-only-redo

  "j" 'evil-next-visual-line ;; I prefer visual line navigation
  "k" 'evil-previous-visual-line ;; "

  "gf" 'xah-open-file-at-cursor
  "f" 'evil-avy-goto-char-in-line

  "/" 'jib/split-window-horizontally-and-switch
  "-" 'jib/split-window-vertically-and-switch  

  "\\" '(lambda () (interactive) (org-agenda nil "c"))
  "|" '(lambda () (interactive) (org-ql-view "Columbia Todo"))
  "]\\" '(lambda () (interactive) (org-agenda nil "w"))
  )

(general-def
  :states '(normal visual motion)
  :keymaps 'override
  "s" 'swiper
  "?" 'evil-search-forward)

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

(general-def
 :keymaps 'emacs
  "C-w C-q" 'kill-this-buffer
 )

(use-package hydra :defer t)

;; This Hydra lets me swich between variable pitch fonts.
(defhydra jb-hydra-variable-fonts (:pre (mixed-pitch-mode 0)
                                     :post (mixed-pitch-mode 1))
  ("t" (set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 160) "Times New Roman")
  ("g" (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160 :weight 'normal) "EB Garamond")
  ("n" (set-face-attribute 'variable-pitch nil :slant 'normal :weight 'normal :height 160 :width 'normal :foundry "nil" :family "Nunito") "Nunito")
  )

(defhydra jb-hydra-theme-switcher (:hint nil)
  "
     Dark                ^Light^
----------------------------------------------
_1_ one              _z_ one-light 
_2_ vivendi          _x_ operandi
_3_ molokai          _c_ jake-plain
_4_ snazzy           _v_ flatwhite
_5_ old-hope         _b_ tomorrow-day
_6_ henna                ^
_7_ kaolin-galaxy        ^
_8_ peacock              ^
_9_ jake-plain-dark      ^
_0_ monokai-machine      ^
_-_ xcode                ^
_q_ quit                 ^
^                        ^
"

  ;; Dark
  ("1" (jib/load-theme 'doom-one)				 "one")
  ("2" (jib/load-theme 'modus-vivendi)			 "modus-vivendi")
  ("3" (jib/load-theme 'doom-molokai)			 "molokai")
  ("4" (jib/load-theme 'doom-snazzy)			 "snazzy")
  ("5" (jib/load-theme 'doom-old-hope)			 "old-hope")
  ("6" (jib/load-theme 'doom-henna)				 "henna")
  ("7" (jib/load-theme 'kaolin-galaxy)			 "kaolin-galaxy")
  ("8" (jib/load-theme 'doom-peacock)			 "peacock")
  ("9" (jib/load-theme 'jake-doom-plain-dark)	 "jake-plain-dark")
  ("0" (jib/load-theme 'doom-monokai-machine)	 "monokai-machine")
  ("-" (jib/load-theme 'doom-xcode)				 "xcode")

  ;; Light
  ("z" (jib/load-theme 'doom-one-light)			 "one-light")
  ("x" (jib/load-theme 'modus-operandi)			 "modus-operandi")
  ("c" (jib/load-theme 'jake-doom-plain)		 "jake-plain")
  ("v" (jib/load-theme 'doom-flatwhite)			 "flatwhite")
  ("b" (jib/load-theme 'doom-opera-light)		 "tomorrow-day")
  ("q" nil))

;; I think I need to initialize windresize to use its commands
;;(windresize)
;;(windresize-exit)

;;(require 'windresize)

;; All-in-one window managment. Makes use of some custom functions,
;; `ace-window' (for swapping), `windmove' (could probably be replaced
;; by evil?) and `windresize'.
;; inspired by https://github.com/jmercouris/configuration/blob/master/.emacs.d/hydra.el#L86
(defhydra jb-hydra-window (:hint nil)
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
   ("P" project-find-file)
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

(defhydra jb-hydra-org-table ()
  "
_c_ insert col    _v_ delete col    Move col: _h_, _l_
_r_ insert row    _d_ delete row    Move row: _j_, _k_
_n_ create table  _i_ create hline
_u_ undo
_q_ quit

"
  ("n" org-table-create "create table")
  ("c" org-table-insert-column "insert col")
  ("r" org-table-insert-row "insert row")
  ("v" org-table-delete-column "delete col")
  ("d" org-table-kill-row "delete row")
  ("i" org-table-insert-hline "hline")

  ("u" undo-fu-only-undo "undo")

  ("h" org-table-move-column-left "move col left")
  ("l" org-table-move-column-right "move col right")
  ("k" org-table-move-row-up "move row up")
  ("j" org-table-move-row-down "move row down")

  ("<left>" org-table-previous-field)
  ("<right>" org-table-next-field)
  ("<up>" previous-line)
  ("<down>" org-table-next-row)

  ("q" nil "quit"))

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-echo-documentation t
        corfu-scroll-margin 0
        corfu-count 8
        corfu-max-width 50
        corfu-min-width corfu-max-width
        corfu-auto-prefix 2)

  ;; Make Evil and Corfu play nice
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)

  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (defun corfu-enable-always-in-minibuffer ()
    (setq-local corfu-auto nil)
    (corfu-mode 1))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" 'corfu-next
            "C-p" 'corfu-previous
            "C-j" 'corfu-next
            "C-k" 'corfu-previous
            "RET" 'corfu-complete
            "<escape>" 'corfu-quit
            ))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; kinda confusing re length, WIP/TODO
  ;; :hook (org-mode . (lambda () (add-to-list 'completion-at-point-functions #'cape-dabbrev)))
  ;; :config
  ;; (setq dabbrev-check-other-buffers nil
  ;;       dabbrev-check-all-buffers nil
  ;;       cape-dabbrev-min-length 6)
  )


(use-package kind-icon
  :config
  (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 1))
  (setq kind-icon-blend-frac 0.08)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'counsel-load-theme #'(lambda () (interactive) (kind-icon-reset-cache)))
  (add-hook 'load-theme         #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-extra-directories nil) ;; Hides . and .. directories
  (setq ivy-initial-inputs-alist nil) ;; Removes the ^ in ivy searches
  (if (eq jib/computer 'laptop)
      (setq-default ivy-height 10)
    (setq-default ivy-height 11))
  (setq ivy-fixed-height-minibuffer t)
  (add-to-list 'ivy-height-alist '(counsel-M-x . 7)) ;; Don't need so many lines for M-x, I usually know what command I want

  (ivy-mode 1)

  ;; Shows a preview of the face in counsel-describe-face
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))

  :general
  (general-define-key
   ;; Also put in ivy-switch-buffer-map b/c otherwise switch buffer map overrides and C-k kills buffers
   :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
   "S-SPC" 'nil
   "C-SPC" 'ivy-restrict-to-matches ;; Default is S-SPC, changed this b/c sometimes I accidentally hit S-SPC
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
  :after ivy
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

  ;; Use fd
  (setq find-program "fd")
  (setq counsel-file-jump-args (split-string "-L --type f -H")) ;; follow symlinks, files, show hidden

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
  (dolist (command '(org-ql-view counsel-find-file fontaine-set-preset))
    (setq ivy-prescient-sort-commands (append ivy-prescient-sort-commands (list command))))
  (ivy-prescient-mode +1))

;; (use-package company-prescient
;;   :defer 2
;;   :after company
;;   :config
;;   (company-prescient-mode +1))

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

;; "Enable Flyspell mode, which highlights all misspelled words. "
(use-package flyspell
  :defer t
  :config
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))

  (dolist (mode '(
                  ;;org-mode-hook
                  mu4e-compose-mode-hook))
    (add-hook mode (lambda () (flyspell-mode 1))))

  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)

  :general ;; Switches correct word from middle click to right click
  (general-define-key :keymaps 'flyspell-mouse-map
                      "<mouse-3>" #'ispell-word
                      "<mouse-2>" nil)
  (general-define-key :keymaps 'evil-motion-state-map
                      "zz" #'ispell-word)
  )

;; (use-package flyspell-correct
;;   :after flyspell
;;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (use-package flyspell-correct-ivy
;;   :after flyspell-correct)

(use-package evil-anzu :defer t)

(use-package avy
  :defer t
  :config
  (setq avy-case-fold-search nil))

(use-package simpleclip :config (simpleclip-mode 1))

;; Allows pasting in minibuffer with M-v
(defun jib/paste-in-minibuffer ()
  (local-set-key (kbd "M-v") 'simpleclip-paste))
(add-hook 'minibuffer-setup-hook 'jib/paste-in-minibuffer)

(use-package undo-fu)

(use-package super-save
  :diminish super-save-mode
  :defer 2
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5 ;; after 5 seconds of not typing autosave
        super-save-triggers ;; Functions after which buffers are saved (switching window, for example)
        '(evil-window-next evil-window-prev balance-windows other-window next-buffer previous-buffer)
        super-save-max-buffer-size 10000000)
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
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(setq text-scale-mode-step 1.2) ;; How much to adjust text scale by when using `text-scale-mode'
(setq jib-default-line-spacing 1) ;; This happens in the variables but I guess I have it here too.

(setq-default line-spacing jib-default-line-spacing)

;; Setting text size based on the computer I am on.
(if (eq jib/computer 'laptopN)
    (setq jib-text-height 140))
(if (eq jib/computer 'desktop)
    (setq jib-text-height 150))

;; (set-frame-font "SF Mono:size=14" nil t)
;; (set-frame-font "Menlo:size=14" nil t)
;; (set-frame-font "Fira Code:size=14" nil t)
(set-frame-font "Roboto Mono:size=14" nil t)

;; (set-face-attribute 'default nil :family "Menlo" :weight 'regular :height jib-text-height)

;; Float height value (1.0) makes fixed-pitch take height 1.0 * height of default
;; This means it will scale along with default when the text is zoomed
;;  (set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :weight 'regular :height 1.0)

;; Height of 160 seems to match perfectly with 12-point on Google Docs
;; (set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 160)

;; (set-face-attribute 'variable-pitch nil :slant 'normal :weight 'normal :height 180 :width 'normal :foundry "nil" :family "Nunito Sans")

;; (set-face-attribute 'variable-pitch nil :slant 'normal :weight 'normal :height 180 :width 'normal :foundry "nil" :family "Nunito Sans")

(use-package mixed-pitch
  :defer t
  :config
  (setq mixed-pitch-set-height nil)
  (dolist (face '(org-date org-priority org-tag org-special-keyword)) ;; Some extra faces I like to be fixed-pitch
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;; Disables showing system load in modeline, useless anyway
(setq display-time-default-load-average nil)

(line-number-mode)
(column-number-mode)
(display-time-mode -1)
(size-indication-mode 0)

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package doom-modeline
  :config
  (doom-modeline-mode)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project ;; Just show file name (no path)
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-icon t ;; Enable/disable all icons
        doom-modeline-modal-icon nil ;; Icon for Evil mode
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-bar-width 3))

;; Configure modeline text height based on the computer I'm on.
;; These variables are used in the Themes section to ensure the modeline
;; stays the right size no matter what theme I use.
(if (eq jib/computer 'laptop)
    (setq jib-doom-modeline-text-height 135) ;; If laptop
  (setq jib-doom-modeline-text-height 140))  ;; If desktop

(if (eq jib/computer 'laptop)
    (setq doom-modeline-height 25) ;; If laptop
  (setq doom-modeline-height 28))  ;; If desktop

(if (or (eq jib/computer 'laptop) (eq jib/computer 'laptopN))
    (setq default-frame-alist '((left . 150)
                                (width . 120)
                                (fullscreen . fullheight)
                                (internal-border-width . 3))))

(if (eq jib/computer 'desktop)
    (setq default-frame-alist '((left . 170)
                                (width . 173)
                                (top . 64)
                                (height . 53)
                                (fullscreen . fullheight)
                                (internal-border-width . 8))))

(use-package all-the-icons)

(use-package doom-themes
  :after mixed-pitch
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom-face
  (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,jib-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,jib-doom-modeline-text-height))))
  ;; (doom-modeline ((t (:height ,jib-doom-modeline-text-height))))
  ;; (doom-modeline-inactive ((t (:height ,jib-doom-modeline-text-height))))
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
  (org-ellipsis ((t (:foreground unspecified :height 0.8 :inherit 'shadow)))))

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t
        modus-themes-region '(bg-only no-extend)
        modus-themes-hl-line '(intense) ;; accented or intense
        modus-themes-syntax '(yellow-comments)
        modus-themes-org-blocks 'gray-background
        modus-themes-mode-line '(moody borderless)) ;; moody or accented is what I use

  ;; (setq modus-themes-headings ;; Makes org headings more colorful
  ;;       '((t . (rainbow))))

  (setq modus-themes-headings
        (quote ((1 . (variable-pitch 1.1 rainbow))
                (2 . (regular))
                (3 . (regular))
                (4 . (regular))
                (t . (rainbow))
                )))
  (modus-themes-load-themes)
  :custom-face
  (org-ellipsis ((t (:height 0.8 :inherit 'shadow))))
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,jib-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,jib-doom-modeline-text-height)))))

(use-package ef-themes
  :init
  (setq ef-themes-headings
        (quote ((1 . (variable-pitch 1.1))
                (2 . (regular))
                (3 . (regular))
                (4 . (regular)))))
  :custom-face
  (org-scheduled-today ((t (:inherit org-level-3)))))


;; loading theme based on the time.
(let ((hour (string-to-number (substring (current-time-string) 11 13))))
  (if (or (> hour 19) (< hour 7))
      (load-theme 'doom-one t) ;; Night
    (load-theme 'doom-opera-light t))) ;; Day

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil) ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'header-line nil :background nil :inherit 'default)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package visual-fill-column
  :defer t
  :config
  (setq visual-fill-column-center-text t)
  (if (eq 'jib/computer 'desktop)
      (setq visual-fill-column-width 100)
    (setq visual-fill-column-width 80))
  (setq visual-fill-column-center-text t))

(use-package writeroom-mode
  :defer t
  :config
  (setq writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-global-effects nil ;; No need to have Writeroom do any of that silly stuff
        writeroom-extra-line-spacing 3) 
  (setq writeroom-width visual-fill-column-width)
  )

(defun my-presentation-on ()
  (setq jib-default-line-spacing 3)
  (setq-default line-spacing jib-default-line-spacing)
  (setq-local line-spacing jib-default-line-spacing)
  (setq ivy-height 6))

(defun my-presentation-off ()
  (jib/reset-var 'jib-default-line-spacing)
  (setq-default line-spacing jib-default-line-spacing)
  (setq-local line-spacing jib-default-line-spacing)
  (jib/reset-var 'ivy-height))

(add-hook 'presentation-on-hook #'my-presentation-on)
(add-hook 'presentation-off-hook #'my-presentation-off)

(if (eq jib/computer 'laptop)
    (setq presentation-default-text-scale 5)
  (setq presentation-default-text-scale 5))

(use-package presentation :defer t)

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil) ;; takes over 'j'
  ;; (setq org-super-agenda-header-prefix " ◦ ") ;; There are some unicode "THIN SPACE"s after the ◦
  ;; Hide the thin width char glyph. This is dramatic but lets me not be annoyed
  (add-hook 'org-agenda-mode-hook
            #'(lambda () (setq-local nobreak-char-display nil)))
  (org-super-agenda-mode))

(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("INPROG-TODO" . 9744)
                                          ("WORK" . 9744)
                                          ("STUDY" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("READ" . 9744)
                                          ("PROJ" . 9744)
                                          ("CONTACT" . 9744)
                                          ("DONE" . 9745)))
  ;; :hook (org-mode . org-superstar-mode)
  )

;; Removes gap when you add a new heading
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; org-modern-star '("●" "○" "✸" "✿")
   org-modern-star '( "⌾" "✸" "◈" "◇")
   org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil))

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
  (setq org-gcal-down-days '20					;; Only fetch events 20 days into the future
        org-gcal-up-days '10					;; Only fetch events 10 days into the past
        org-gcal-recurring-events-mode 'top-level
        org-gcal-remove-api-cancelled-events t) ;; No prompt when deleting removed events
  ;; NOTE - org-gcal ids and calendar configuation is set in 'private.el' for sake of security/privacy.
  )

(use-package org-appear
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t		;; A default setting that needs to be t for org-appear
        org-appear-autoemphasis t		;; Enable org-appear on emphasis (bold, italics, etc)
        org-appear-autolinks nil		;; Don't enable on links
        org-appear-autosubmarkers t))	;; Enable on subscript and superscript

(use-package ox-reveal
  :defer 5)

(setq org-modules '(org-habit))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package org-ql
  :defer t
  :general
  (general-define-key :keymaps 'org-ql-view-map
                      "q" 'kill-buffer-and-window)
  )

(use-package org-preview-html
  :defer t
  :config
  (setq org-preview-html-viewer 'xwidget))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-latex-create-formula-image-program 'dvisvgm) ;; sharper
  (plist-put org-format-latex-options :scale 1.5) ;; bigger
  (setq org-latex-preview-ltxpng-directory (concat (temporary-file-directory) "ltxpng/"))
  )

(use-package org-tree-slide
  :defer t
  :config
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-skip-outline-level 3))

(use-package org-download
  :defer 2
  :config
  (setq org-download-method 'attach)
  (advice-add 'org-download-yank :before 'jib/system-clipboard-to-emacs-clipboard))

(use-package valign :defer t)

(setq org-special-ctrl-a/e t)

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "t" 'org-todo
  "<return>" 'org-open-at-point-global
  "K" 'org-shiftup
  "J" 'org-shiftdown
  "<f5>" 'org-ctrl-c-ctrl-c)

(general-def
  :states 'insert
  :keymaps 'org-mode-map
  "C-o" 'evil-org-open-above
  "S-<left>" 'org-shiftleft
  "S-<right>" 'org-shiftright)

(general-def
  :keymaps 'org-mode-map
  "M-[" 'org-metaleft
  "M-]" 'org-metaright
  "C-M-=" 'ap/org-count-words
  "s-r" 'org-refile
  "M-k" 'org-insert-link
  "C-c t" 'jib/org-done-keep-todo)

;; Org-src - when editing an org source block
(general-def
  :prefix ","
  :states 'normal
  :keymaps 'org-src-mode-map
  "b" '(nil :which-key "org src")
  "bc" 'org-edit-src-abort
  "bb" 'org-edit-src-exit)

(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-mode-map)
 "" nil
 "A" '(org-archive-subtree-default :which-key "org-archive")
 "a" '(org-agenda :which-key "org agenda")
 "6" '(org-sort :which-key "sort")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-schedule :which-key "schedule")
 "S" '(jib/org-schedule-tomorrow :which-key "schedule tmrw")
 "d" '(org-deadline :which-key "deadline")
 "g" '(counsel-org-goto :which-key "goto heading")
 "t" '(counsel-org-tag :which-key "set tags")
 "p" '(org-set-property :which-key "set property")
 "r" '(jib/org-refile-this-file :which-key "refile in file")
 "e" '(org-export-dispatch :which-key "export org")
 "," '(jib/org-set-startup-visibility :which-key "startup visibility")
 "." '(org-toggle-narrow-to-subtree :which-key "toggle narrow to subtree")
 "H" '(org-html-convert-region-to-html :which-key "convert region to html")
 "C" '(jib/org-copy-link-to-clipboard :which-key "copy link to clipboard")
 "=" '(ap/org-count-words :which-key "ap/org-count-words")

 "1" '(org-toggle-link-display :which-key "toggle link display")
 "2" '(org-toggle-inline-images :which-key "toggle images")
 "3" '(jib/org-occur-unchecked-boxes :which-key "occur unchecked boxes")

 "b" '(nil :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle")
 "bb" '(org-edit-special :which-key "org-edit-special")
 "bc" '(org-edit-src-abort :which-key "org-edit-src-abort")
 "bk" '(org-babel-remove-result-one-or-many :which-key "org-babel-remove-result-one-or-many")

 "x" '(nil :which-key "text")
 "xb" (spacemacs|org-emphasize spacemacs|org-bold ?*)
 "xb" (spacemacs|org-emphasize spacemacs|org-bold ?*)
 "xc" (spacemacs|org-emphasize spacemacs|org-code ?~)
 "xi" (spacemacs|org-emphasize spacemacs|org-italic ?/)
 "xs" (spacemacs|org-emphasize spacemacs|org-strike-through ?+)
 "xu" (spacemacs|org-emphasize spacemacs|org-underline ?_)
 "xv" (spacemacs|org-emphasize spacemacs|org-verbose ?~) ;; I realized that ~~ is the same and better than == (Github won't do ==)

 ;; insert
 "i" '(nil :which-key "insert")

 "il" '(org-insert-link :which-key "org-insert-link")
 "l" '(org-insert-link :which-key "org-insert-link") ;; More convenient access
 "iL" '(counsel-org-link :which-key "counsel-org-link")
 "it" '(jb-hydra-org-table/body :which-key "tables")

 "is" '(nil :which-key "insert stamp")
 "iss" '((lambda () (interactive) (call-interactively (org-time-stamp-inactive))) :which-key "org-time-stamp-inactive")
 "isS" '((lambda () (interactive) (call-interactively (org-time-stamp nil))) :which-key "org-time-stamp")

 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-clock-in :which-key "clock in")
 "co" '(org-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )


;; Org-agenda
(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-agenda-mode-map)
 "" nil
 "a" '(org-agenda :which-key "org agenda")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-agenda-schedule :which-key "schedule")
 "," '(org-agenda-schedule :which-key "schedule") ;; quick access
 "d" '(org-agenda-deadline :which-key "deadline")
 "t" '(org-agenda-set-tags :which-key "set tags")
 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-agenda-clock-in :which-key "clock in")
 "co" '(org-agenda-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )

(evil-define-key 'motion org-agenda-mode-map
  (kbd "f") 'org-agenda-later
  (kbd "b") 'org-agenda-earlier)

(defface jib-read
  '((t (:foreground "MediumPurple2")))
  "Custom face for highlighting read."
  :group 'jib)

;; (defun jib/org-highlight-setup ()
;;   (highlight-regexp "\\<\\(TODO\\|NEXT\\) \\(Read\\)\\>" 'jib-read 2))

;; (add-hook 'org-agenda-finalize-hook 'jib/org-highlight-setup)
;; (add-hook 'org-mode-hook 'jib/org-highlight-setup)

(defun jib/org-setup ()
  (org-indent-mode) ;; Keeps org items like text under headings, lists, nicely indented
  (visual-line-mode 1) ;; Nice line wrapping
  (centered-cursor-mode) ;; Enable centered cursor mode
  (smartparens-mode 0) ;; Disable smartparents
  (hl-prog-extra-mode) ;; Highlighting with regexps
  (setq-local line-spacing (+ jib-default-line-spacing 2)) ;; A bit more line spacing for orgmode
  (valign-mode)
  )

(use-package org
  ;; :pin elpa
  :hook (org-mode . jib/org-setup)
  :hook (org-mode . jib/prettify-symbols-setup)
  :hook (org-capture-mode . evil-insert-state) ;; Start org-capture in Insert state by default
  :diminish org-indent-mode
  :diminish visual-line-mode
  :config

(setq org-ellipsis "…")
;; ⤵ ▼ ⬎  
(setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
(setq org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX
(setq org-startup-folded 'showeverything)
(setq org-image-actual-width 300)
(setq org-fontify-whole-heading-line t)
(setq org-pretty-entities t)

(setq org-cycle-separator-lines 1)
(setq org-catch-invisible-edits 'show-and-error) ;; 'smart
(setq org-src-tab-acts-natively t)

;; M-Ret can split lines on items and tables but not headlines and not on anything else (unconfigured)
(setq org-M-RET-may-split-line '((headline) (item . t) (table . t) (default)))
(setq org-loop-over-headlines-in-active-region nil)

;; Opens links to other org file in same frame (rather than splitting)
(setq org-link-frame-setup '((file . find-file)))

(setq org-log-done t
      org-log-into-drawer t)

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

(defun jib/post-org-goto ()
  (let ((current-prefix-arg '(4))) ;; emulate C-u
    (call-interactively 'org-reveal))
  (org-cycle))

(advice-add 'counsel-org-goto :after #'jib/post-org-goto)
(advice-add 'org-agenda-goto :after #'jib/post-org-goto)
(advice-add 'org-agenda-switch-to :after #'jib/post-org-goto)

;; (setq org-tag-faces '(
;;                       ("Misc" . "tan1")
;;                       ("qp" . "RosyBrown1") ;; Quick-picks
;;                       ("ec" . "PaleGreen3") ;; Extracurricular
;;                       ("st" . "DimGrey") ;; Near-future (aka short term) todo
;;                       ))

(setq org-tags-column -1)

(setq org-todo-keywords '((type
                           "TODO(t)" "WAITING(h)" "INPROG-TODO(i)" "WORK(w)"
                           "STUDY(s)" "SOMEDAY" "READ(r)" "PROJ(p)" "CONTACT(c)"
                           "|" "DONE(d)" "CANCELLED(C@)")))

(setq org-todo-keyword-faces
      '(("TODO"  :inherit (region org-todo) :foreground "DarkOrange1"   :weight bold)
        ("WORK"  :inherit (org-todo region) :foreground "DarkOrange1"   :weight bold)
        ("READ"  :inherit (org-todo region) :foreground "MediumPurple2" :weight bold)
        ("PROJ"  :inherit (org-todo region) :foreground "orange3"     :weight bold)
        ("STUDY" :inherit (region org-todo) :foreground "plum3"       :weight bold)
        ("DONE" . "SeaGreen4")))

(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 . "red2")
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
   (java . t)
   ))

(use-package gnuplot :defer t)

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
(setq python-shell-completion-native-enable nil)

;; How to open buffer when calling `org-edit-special'.
(setq org-src-window-setup 'current-window)

(setq org-habit-preceding-days 6
      org-habit-following-days 6
      org-habit-show-habits-only-for-today nil
      org-habit-today-glyph ?⍟ ;;‖
      org-habit-completed-glyph ?✓
      org-habit-graph-column 40)

;; custom time stamp format. I don't use this.
(setq org-time-stamp-custom-formats '("<%A, %B %d, %Y" . "<%m/%d/%y %a %I:%M %p>"))

(setq org-agenda-restore-windows-after-quit t)

(setq org-agenda-window-setup 'current-window)

;; Only show upcoming deadlines for the next X days. By default it shows
;; 14 days into the future, which seems excessive.
(setq org-deadline-warning-days 3)
;; If something is done, don't show its deadline
(setq org-agenda-skip-deadline-if-done t)
;; If something is done, don't show when it's scheduled for
(setq org-agenda-skip-scheduled-if-done t)
;; If something is scheduled, don't tell me it is due soon
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; use AM-PM and not 24-hour time
(setq org-agenda-timegrid-use-ampm t)

;; A new day is 3am (I work late into the night)
;; (setq org-extend-today-until 3)

;; (setq org-agenda-time-grid '((daily today require-timed)
;;                              (1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
;;                              "        " "----------------"))

(setq org-agenda-time-grid nil)

;; (setq org-agenda-span 'day)

(setq org-agenda-block-separator ?-)
;; (setq org-agenda-current-time-string "<----------------- Now")

;; ;; (setq org-agenda-block-separator nil)

;; (setq org-agenda-scheduled-leaders '("Plan | " "Sched.%2dx: ") ; ⇛
;;       org-agenda-deadline-leaders '("Due: " "(in %1d d.) " "Due %1d d. ago: "))

;; (setq org-agenda-prefix-format '((agenda . "  %-6:T %t%s")
;;                                  (todo . "  %-6:T %t%s")
;;                                  (tags . " %i %-12:c")
;;                                  (search . " %i %-12:c")))

;;;;; more true to defaults

(setq org-agenda-prefix-format '((agenda . " %-12:T%?-12t% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(setq org-agenda-deadline-leaders '("Deadline:  " "In %2d d.: " "%2d d. ago: "))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (setq-local line-spacing 3)))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (hide-mode-line-mode)))

(setq org-agenda-custom-commands nil)

(setq org-agenda-hide-tags-regexp "\\(ec\\|lit\\|sci\\|edu\\|ds\\|calc3\\)")

(defvar jib-org-agenda-columbia-productivity-super-groups
  '((:name "Personal Items" :tag "p" :order 10)
    (:name "Extracurricular" :tag "ec" :order 5)
    (:name "Todo" :todo ("TODO") :order 3)
    (:name "Heads Up!"
           :todo ("PROJ" "WORK" "STUDY") :tag "lt" :order 4)
    (:discard (:todo t))))

(defvar jib-org-columbia-productivity-ql-query
  '(and (not (tags "defer"))
        (not (scheduled)) ;; rationale --- if it's scheduled I don't need the heads-up
        (or (effort 1)
            (todo "TODO" "PROJ" "STUDY")
            (and (todo)
                 (tags "p" "ec" "lt")))))

;; Day View
(add-to-list 'org-agenda-custom-commands
             '("c" "Columbia Day View"
               ((agenda "" ((org-agenda-overriding-header "Columbia Productivity View")
                            (org-agenda-span 'day)
                            (org-agenda-sorting-strategy '(scheduled-up deadline-up priority-down))
                            (org-super-agenda-groups '(
                                                       (:name "Today:"
                                                              :scheduled t
                                                              :order 2)
                                                       (:name "Deadlines:"
                                                              :deadline t
                                                              :order 3)
                                                       (:name "Today's Schedule:"
                                                              :time-grid t
                                                              :discard (:deadline t)
                                                              :order 1)))))

                (org-ql-block jib-org-columbia-productivity-ql-query
                              ((org-ql-block-header "Productivity Overview:")
                               (org-super-agenda-groups jib-org-agenda-columbia-productivity-super-groups))))))

;; Day View No Agenda
(add-to-list 'org-agenda-custom-commands
             '("v" "Columbia Day View No Agenda"
               ((org-ql-block '(todo)
                              ((org-super-agenda-groups (push '(:name "Today's Tasks" ;; jib-org-super-agenda-school-groups, with this added on
                                                                      :scheduled today
                                                                      :deadline today) jib-org-agenda-columbia-productivity-super-groups)

                                                        ;; '((:name "Today's Tasks"
                                                        ;;                                  :scheduled today
                                                        ;;                                  :deadline today)
                                                        ;;                           (:discard (:tag "defer"))
                                                        ;;                           (:name "Extracurricular:"
                                                        ;;                                  :tag "ec"
                                                        ;;                                  :order 10)
                                                        ;;                           (:name "Personal:"
                                                        ;;                                  :tag "p"
                                                        ;;                                  :order 5)
                                                        ;;                           (:name "Projects"
                                                        ;;                                  :todo ("STUDY" "PROJ")
                                                        ;;                                  :tag "lt")
                                                        ;;                           (:discard (:todo t)))
                                                        ))))))

;; Three-day view
(add-to-list 'org-agenda-custom-commands
             '("w" "Columbia Four-Day View"
               ((agenda "" ((org-agenda-span 4)
                            (org-agenda-entry-types '(:deadline :scheduled))
                            (org-agenda-start-on-weekday nil)
                            (org-deadline-warning-days 0)))

                (org-ql-block jib-org-columbia-productivity-ql-query
                              ((org-ql-block-header "Productivity Overview:")
                               (org-super-agenda-groups jib-org-agenda-columbia-productivity-super-groups))))))

;; Six-day view
(add-to-list 'org-agenda-custom-commands
             '("q" "Columbia Ten-Day View"
               ((agenda "" ((org-agenda-span 10)
                            (org-agenda-entry-types '(:deadline :scheduled))
                            (org-agenda-start-on-weekday nil)
                            (org-deadline-warning-days 0))))))

;; This isn't super needed as I mostly just use my custom refile command
;; to refile to only the current buffer.
;; (setq org-refile-targets (quote (("~/Dropbox/org/work.org" :maxlevel . 2)
;;                                  ("~/Dropbox/org/cpb.org"  :maxlevel . 8)
;;                                  ;; ("~/Dropbox/notes/columbia/columbia_inbox.org")
;;                                  )))

;; (setq org-outline-path-complete-in-steps t) ; Refile in a single go
;; (setq org-refile-use-outline-path t)          ; Show full paths for refiling

;; By default an org-capture/refile will save a bookmark. This
;; disables that and keeps my bookmark list how I want it.
(setq org-bookmark-names-plist nil)


(setq org-capture-templates
      '(
        ("n" "CPB Note" entry (file+headline "~/Dropbox/org/cpb.org" "Refile")
         "** NOTE: %? @ %U"        :empty-lines 0 :refile-targets (("~/Dropbox/org/cpb.org" :maxlevel . 8)))

        ("i" "CPB Idea" entry (file+headline "~/Dropbox/org/cpb.org" "Refile")
         "** IDEA: %? @ %U :idea:" :empty-lines 0 :refile-targets (("~/Dropbox/org/cpb.org" :maxlevel . 8)))

        ("m" "CPB Note Clipboard")

        ("mm" "Paste clipboard" entry (file+headline "~/Dropbox/org/cpb.org" "Refile")
         "** NOTE: %(simpleclip-get-contents) %? @ %U" :empty-lines 0 :refile-targets (("~/Dropbox/org/cpb.org" :maxlevel . 8)))

        ("ml" "Create link and fetch title" entry (file+headline "~/Dropbox/org/cpb.org" "Refile")
         "** [[%(simpleclip-get-contents)][%(jib/www-get-page-title (simpleclip-get-contents))]] @ %U" :empty-lines 0 :refile-targets (("~/Dropbox/org/cpb.org" :maxlevel . 8)))

        ("w" "Work Todo Entries")
        ("we" "No Time" entry (file "~/Dropbox/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title} %?" :prepend t :empty-lines-before 0
         :refile-targets (("~/Dropbox/org/work.org" :maxlevel . 2)))

        ("ws" "Scheduled" entry (file "~/Dropbox/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
         :refile-targets (("~/Dropbox/org/work.org" :maxlevel . 2)))

        ("wd" "Deadline" entry (file "~/Dropbox/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
         :refile-targets (("~/Dropbox/org/work.org" :maxlevel . 2)))

        ("ww" "Scheduled & deadline" entry (file "~/Dropbox/org/work.org")
         "** %^{Type|HW|READ|TODO|PROJ} %^{Todo title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0
         :refile-targets (("~/Dropbox/org/work.org" :maxlevel . 2)))

        ("t" "Temp file entry" entry (file "~/Dropbox/.tmp.org")
         "** %^{Heading} @ %u \n%?" :prepend t)

        ))

;; (setq org-export-backends '(ascii beamer html latex md odt))

(setq org-export-with-broken-links t
      org-export-with-smart-quotes t
      org-export-allow-bind-keywords t)

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

(use-package ox-moderncv
  :ensure nil
  :init (require 'ox-moderncv))

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
(setq org-clock-clocked-in-display 'both)

(setq org-attach-id-dir ".org-attach/"
      org-attach-use-inheritance t)


) ;; This parenthesis ends the org use-package.

(use-package magit :defer t)
(use-package unfill :defer t)
(use-package burly :defer t)
(use-package ace-window :defer t)
(use-package centered-cursor-mode :diminish centered-cursor-mode)
(use-package restart-emacs :defer t)
(use-package diminish)
(use-package reveal-in-osx-finder :commands (reveal-in-osx-finder))

(use-package bufler
  :config
  (setq bufler-filter-buffer-modes nil ;; Don't hide so many buffers
        bufler-filter-buffer-name-regexps nil)
  (setf bufler-groups (bufler-defgroups
                        (group
                         ;; Subgroup collecting all named workspaces.
                         (auto-workspace))
                        (group
                         ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
                         (group-or "*Help/Info*"
                                   (mode-match "*Help*" (rx bos "help-"))
                                   (mode-match "*Info*" (rx bos "info-"))))
                        (group
                         ;; Subgroup collecting all special buffers (i.e. ones that are not
                         ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
                         ;; through to other groups, so they end up grouped with their project buffers).
                         (group-and "*Special*"
                                    (lambda (buffer)
                                      (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                                           buffer)
                                                  (funcall (mode-match "Dired" (rx bos "dired"))
                                                           buffer)
                                                  (funcall (auto-file) buffer))
                                        "*Special*")))
                         (group
                          ;; Subgroup collecting these "special special" buffers
                          ;; separately for convenience.
                          (name-match "**Special**"
                                      (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
                         (group
                          ;; Subgroup collecting all other Magit buffers, grouped by directory.
                          (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
                          (auto-directory))
                         ;; Remaining special buffers are grouped automatically by mode.
                         ;; (auto-mode)
                         )
                        (group (dir "~/Dropbox/notes/")
                               (auto-mode)
                               (auto-directory))
                        (group (dir "~/Dropbox/org/"))
                        ;; Group remaining buffers by directory, then major mode.
                        (auto-directory)
                        (auto-mode)))
  :general
  (:keymaps 'bufler-list-mode-map "Q" 'kill-this-buffer))

(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :config
  (setq hl-prog-extra-list
        (list
         '("\\<\\(TODO\\|NOTE\\)\\(([^)+]+)\\)?" 0 comment
           (:weight bold :inherit diff-removed))
         ;; Match TKs in quotation marks (hl-prog-extra sees them as strings)
         '("\\(TK\\)+" 0 string '(:weight bold :inherit font-lock-warning-face))
         ;; Match TKs not in quotation marks
         '("\\(TK\\)+" 0 nil '(:weight bold :inherit font-lock-warning-face))))
  (global-hl-prog-extra-mode))

(use-package xwidget
  :general
  (general-define-key :states 'normal :keymaps 'xwidget-webkit-mode-map 
                      "j" 'xwidget-webkit-scroll-up-line
                      "k" 'xwidget-webkit-scroll-down-line
                      "gg" 'xwidget-webkit-scroll-top
                      "G" 'xwidget-webkit-scroll-bottom))

(use-package mw-thesaurus
  :defer t
  :config
  (add-hook 'mw-thesaurus-mode-hook
            (lambda () (define-key evil-normal-state-local-map (kbd "q") 'mw-thesaurus--quit))))

(use-package ansi-term
  :ensure nil
  :general
  (:keymaps 'term-mode-map
            "<up>" 'term-previous-input
            "<down>" 'term-next-input))

;; https://github.com/oantolin/epithet
(use-package epithet
  :ensure nil
  :config
  (add-hook 'Info-selection-hook #'epithet-rename-buffer)
  (add-hook 'help-mode-hook #'epithet-rename-buffer))

;; https://github.com/udyantw/most-used-words
(use-package most-used-words :ensure nil)

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

;; (use-package company-auctex
;;   :after auctex
;;   :init
;;   (add-to-list 'company-backends 'company-auctex)
;;   (company-auctex-init))

(use-package pdf-tools
  :defer t
  ;; stop pdf-tools being automatically updated when I update the
  ;; rest of my packages, since it would need the installation command and restart
  ;; each time it updated.
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

                      "C-s" 'isearch-forward
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

(use-package rainbow-mode :defer t)

(use-package quickrun :defer t
  :general
  (general-define-key :states 'normal :keymaps 'quickrun--mode-map
                      "q" 'quit-window
                      "s-9" 'quickrun-shell))

;; A better python mode (supposedly)
(use-package python-mode
  :defer t)

(general-define-key :states '(emacs) :keymaps 'inferior-python-mode-map
                    "<up>" 'comint-previous-input
                    "<down>" 'comint-next-input)

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

(use-package emacs-lisp-mode
  :ensure nil
  :general
  (general-define-key
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

   "b" '(nil :which-key "org src")
   "bc" 'org-edit-src-abort
   "bb" 'org-edit-src-exit
   )
  )
