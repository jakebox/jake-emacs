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

(defun jib/open-buffer-file-mac ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

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
  (let ((newbuf (generate-new-buffer-name "*scratch*")))
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

(defun jib/split-and-close-sentence ()
  "Deletes current word, inserts a period, and capitalizes the next word -
splits the sentence."
  (interactive)
  (kill-word 1)
  (delete-backward-char 1)
  (insert ".")
  (capitalize-word 1))

(defun jib/fzf ()
  "Allows you to select a folder to fzf"
  (interactive)
  (let ((current-prefix-arg '-)) ;; emulate C-u
    (call-interactively 'counsel-fzf)))

(defun jib/calc-speaking-time ()
  "Calculate how long it would take me to speak aloud the selection."
  (interactive)
  (if (use-region-p) (let* ((wpm 150)
							(word-count (float (count-words-region (region-beginning) (region-end))))
							(raw-time (* 60 (/ word-count wpm))))
					   (message "%s minutes, %s seconds to speak at %d wpm"
								(format-seconds "%m" raw-time)
								(floor(mod raw-time 60)) wpm))
	(error "Error: select a region.")))

(defun jib/listify (&optional count)
  "Turn the region's (or count = n lines) into an orgmode list by appending a +."
  (interactive "p")
  (let ((lines (count-lines (region-beginning) (region-end)))) ;; By default grab a region
	(if (> count 1) (setq lines count)) ;; but if there was an argument, override the # of lines
	(save-excursion
	  (if (use-region-p) ;; If there's a region go to the start and deactivate the region
		  (goto-char (region-beginning)) (deactivate-mark))
	  (while (> lines 0) ;; Add "+ " at the beginning of each line
		(beginning-of-line)
		(insert "+ ")
		(next-line)
		(setq lines (1- lines))))))

(defun jib/org-tmp-html-export ()
  "Export the current org file to a temporary HTML file and open."
  (interactive)
  (save-window-excursion
	(org-html-export-as-html)
	(write-file (concat (make-temp-file "jibemacsorg") ".html"))
	(jib/open-buffer-file-mac)
	(kill-this-buffer)))

(defun jib/org-tmp-html-export-region ()
  "Temp HTML of current org region in browser."
  (interactive)
  (let ((old-buffer (current-buffer)) (beg (region-beginning)) (end (region-end)))
	(with-temp-buffer
	  (insert "#+OPTIONS: toc:nil date:nil author:nil num:nil title:nil tags:nil \
	  todo:nil html-link-use-abs-url:nil html-postamble:nil html-preamble:nil \
	  html-scripts:nil html-style:nil html5-fancy:nil tex:nil")
      (insert-buffer-substring old-buffer (- beg 1) end)
	  (jib/org-tmp-html-export))))

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


(defun jib/save-and-close-this-buffer (buffer)
  "Saves and closes given buffer."
  (if (get-buffer buffer)
	  (let ((b (get-buffer buffer)))
		(save-buffer b)
		(kill-buffer b))))

(defun jib/org-schedule-tomorrow ()
  "Org Schedule for tomorrow (+1d)."
  (interactive)
  (org-schedule t "+1d"))

(defun jib/org-set-startup-visibility ()
  (interactive)
  (org-set-startup-visibility))

;; Modified from https://stackoverflow.com/questions/25930097/emacs-org-mode-quickly-mark-todo-as-done?rq=1
(defun jib/org-done-keep-todo ()
  "Mark an org todo item as done while keeping its former keyword intact, and archive.

For example, * TODO This item    becomes    * DONE TODO This item. This way I can see what
the todo type was if I look back through my archive files."
  (interactive)
  (let ((state (org-get-todo-state)) (todo (org-entry-get (point) "TODO"))
        post-command-hook)
    (if (not (eq state nil))
        (progn (org-todo "DONE")
			   (beginning-of-line)
			   (forward-word)
			   (insert (concat " " todo))
			   (org-archive-subtree-default))
	  (user-error "Not a TODO."))
    (run-hooks 'post-command-hook)))

(general-define-key :keymaps 'org-mode-map "C-c t" 'jib/org-done-keep-todo)


;; ------ MACROS ------ ;


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
