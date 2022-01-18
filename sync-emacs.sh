#!/usr/local/bin/emacs --script

;; Script to copy files in jake-emacs remote directory to local ~/.emacs.default
;; Only needs to be run once on a machine

(setq-local command "ln -s ")
(setq-local target "~/.emacs.default/")
(setq-local jake-emacs (expand-file-name "jake-emacs/" default-directory))

(setq-local files (directory-files "jake-emacs/" nil "\\(.el\\|.org\\)"))

(cd target)

(while files ;; Loop through files
  (shell-command (concat command jake-emacs (car files) " " target (car files)))
  (setq files (cdr files)))
