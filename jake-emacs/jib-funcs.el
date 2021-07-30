;; -*- lexical-binding: t; -*-
;;; 
;;; Jake B Functions File
;;;

;; Copyright (C) Jake B
;; Author: Jake B <message on GitHub for further contact>
;; URL: https://github.com/jakebox/.emacs
;; This file is not part of GNU Emacs.
;; This file is free software.

;; JIB Functions & Variables

(setq jib/home (concat (getenv "HOME") "/"))

;; Referring to external (Dropbox) locations/files
(setq jib/dropbox (concat jib/home "Dropbox/"))
(setq org-directory (concat jib/dropbox "org"))
(setq jib/emacs-stuff (concat jib/dropbox "Mackup/emacs-stuff"))

;; At this point things can be local now
(setq jib/init.org (expand-file-name "init.org" user-emacs-directory))


(defun jib/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))


(defun jib/edit-init ()
  (interactive)
  (find-file-existing jib/init.org))

;; Automatically tangle our Emacs.org config file when we save it
;; from emacs-from-scratch
(defun jib/org-babel-tangle-config ()
  (interactive)
  (when (string-equal (buffer-file-name)
                      jib/init.org)
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))


(defun jib/org-agenda-caller (letter)
  "Calls a specific org agenda view specified by the letter argument."
  (interactive)
  (org-agenda nil letter))


(defun jib/reload-emacs-configuration ()
  (interactive)
  (load (expand-file-name "init.el" user-emacs-directory)))

;; From spacemacs
(defun spacemacs/insert-line-below-no-indent (count)
  "Insert a new line below with no indentation."
  (interactive "p") (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

(defun spacemacs/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun jib/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))


(defun spacemacs/deft ()
  "Helper to call deft and then fix things so that it is nice and works"
  (interactive)
  (deft)
  ;; Hungry delete wrecks deft's DEL override
  (when (fboundp 'hungry-delete-mode)
    (hungry-delete-mode -1))
  ;; When opening it you always want to filter right away
  (evil-insert-state nil))

;; from https://gist.github.com/3402786
(defun jib/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;; from https://gist.github.com/timcharper/493269
(defun jib/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun jib/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))


(defmacro spacemacs|org-emphasize (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
          (org-emphasize ,char)))

;; From http://blog.binchen.org/posts/the-reliable-way-to-access-system-clipboard-from-emacs.html
;; Uses simpleclip
(defun jib/paste-in-minibuffer ()
  (local-set-key (kbd "M-v") 'simpleclip-paste))

;; Uses simpleclip
(defun jib/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (mark-whole-buffer)
  (simpleclip-copy (point-min) (point-max))
  (deactivate-mark))

(defun jib/fzf ()
  "Allows you to select a folder to fzf"
  (interactive)
  (let ((current-prefix-arg '-)) ;; emulate C-u
    (call-interactively 'counsel-fzf)))

;; DOES NOT WORK B/C I ARCHIVE TO MORE THAN JUST MY HOMEWORK-ARCHIVE FOLDER. WILL TRY TO WORK ON
;; EVENTUALLY.

;; ;; When archiving something to my homework archive, save the archive file and kill the buffer.
;; ;; I rarely need to leave the buffer open and it just bothers me to have it in the list or to have to
;; ;; save it manually. Adapted from: https://matthewbilyeu.com/blog/2019-09-28/auto-saving-org-archive-file
;; (defconst homework-archive-file (concat org-directory "/org-archive/homework-archive.org_archive"))
;; (defun save-homework-archive-file ()
;;   (save-some-buffers 'no-confirm (lambda ()
;;                                    (equal buffer-file-name
;;                                           (expand-file-name homework-archive-file))))
;;   (kill-buffer "homework-archive.org_archive"))
;; (advice-add 'org-archive-subtree-default :after #'save-homework-archive-file)


(defun jib/calc-speaking-time ()
  "Calculate how long it would take me to speak aloud the selection."
  (interactive)
  (if (use-region-p)
	  (let ((word-count (float (count-words-region (region-beginning) (region-end)))))
		(let ((raw-time (/ word-count 150)))
		  (message "%d minutes, %d seconds" (floor raw-time) (round (* 60 (- raw-time (floor raw-time)))))))))


;; https://www.reddit.com/r/emacs/comments/8qm1lb/new_orgcountwords_command/
(defun ap/org-forward-to-entry-content (&optional unsafe)
    "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
    (unless unsafe
      ;; To improve performance in loops (e.g. with `org-map-entries')
      (org-back-to-heading))
    (cl-loop for element = (org-element-at-point)
             for pos = (pcase element
                         (`(headline . ,_) (org-element-property :contents-begin element))
                         (`(,(or 'planning 'property-drawer 'drawer) . ,_) (org-element-property :end element)))
             while pos
             do (goto-char pos)))

(defun ap/org-count-words ()
    "If region is active, count words in it; otherwise count words in current subtree."
    (interactive)
    (if (use-region-p)
        (funcall-interactively #'count-words-region (region-beginning) (region-end))
      (org-with-wide-buffer
       (cl-loop for (lines words characters)
                in (org-map-entries
                    (lambda ()
                      (ap/org-forward-to-entry-content 'unsafe)
                      (let ((end (org-entry-end-position)))
                        (list (count-lines (point) end)
                              (count-words (point) end)
                              (- end (point)))))
                    nil 'tree)
                sum lines into total-lines
                sum words into total-words
                sum characters into total-characters
                finally do (message "Subtree \"%s\" has %s lines, %s words, and %s characters."
                                    (org-get-heading t t) total-lines total-words total-characters)))))


;; ------ MACROS ------ ;;


;; Converts org mode from current line to bottom to HTML and copies it to the system clipboard
;; uses org-html-convert-region-to-html
(fset 'jib|Brinkley-HTML
   (kmacro-lambda-form [?V ?G ?y ?  ?f ?n ?  ?h ?M ?O ?p ?V ?G ?, ?H ?  ?x ?C ?  ?b ?d] 0 "%d"))

;; Takes an org mode list and adds bullets one indent in under each item
(fset 'jib|Listify
   (kmacro-lambda-form [?0 ?i ?+ ?\S-  escape ?j] 0 "%d"))

;; Takes a single-leveled org mode list and adds a sub item under each item
(fset 'jib|SubListify
      (kmacro-lambda-form [?A M-return tab S-right escape ?j ?0] 0 "%d"))


;; WIP
(defun jib/classy-org-setup ()
  (interactive)
  (org-num-mode)
  (org-superstar-mode 0)
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil)))))
  )
