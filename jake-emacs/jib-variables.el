;; jib-variables.el

;; File/directory locations --------
(defvar jib/home (concat (getenv "HOME") "/") "My home directory.")
(defvar jib/dropbox (concat jib/home "Dropbox/") "The parent Dropbox folder.")
;; (defvar org-directory (concat jib/dropbox "org") "Directory with org files.")
(setq org-directory (concat jib/dropbox "org"))
(defvar jib/emacs-stuff (concat jib/dropbox "files/systems/emacs-stuff") "Directory where personal Emacs files are kept")

;; Internal use variables
(defvar jib/computer 'laptop "Which computer I am on -- 'desktop or 'laptop.")
(defvar jib-text-height nil "My preferred default text height.")
(defvar jib-doom-modeline-text-height nil "My preferred modeline text height.")
(defvar jib-default-line-spacing 0 "Baseline line spacing.")
(setq-default jib-default-line-spacing 0)

;; Emacs variables
(setq bookmark-default-file (concat jib/emacs-stuff "/bookmarks"))
(setq custom-theme-directory (expand-file-name "themes" jib/emacs-stuff))


(provide 'jib-variables)
;;; jib-variables.el ends here
