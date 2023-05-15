;; -*- lexical-binding: t; -*-
;;; 
;;; Jake B Functions File
;;;

;; Copyright (C) Jake B
;; Author: Jake B <jakebox0@protonmail.com>
;; URL: https://github.com/jakebox/.emacs
;; This file is not part of GNU Emacs.
;; This file is free software.

;; JIB Functions

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; from https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically0
(defun jib/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

;;;;;;;;;;;;;;;;;;;;;;
;; Window Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun jib/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))
(defun jib/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))
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


;;;;;;;;;;;;;;;;;;;;;;;
;; Files and Buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun jib/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun jib/save-and-close-this-buffer (buffer)
  "Saves and closes given buffer."
  (if (get-buffer buffer)
	  (let ((b (get-buffer buffer)))
		(save-buffer b)
		(kill-buffer b))))

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

(defun spacemacs/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "*scratch*")))
    (switch-to-buffer newbuf)))

;; from magnars
(defun spacemacs/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun jib/split-and-scratch-elisp ()
  "Split window and create new buffer for Emacs Lisp evaluation."
  (interactive)
  (jib/split-window-horizontally-and-switch)
  (spacemacs/new-empty-buffer)
  (emacs-lisp-mode))

(defun jib/reload-emacs-configuration ()
  (interactive)
  (load (expand-file-name "init.el" user-emacs-directory)))

(defun jib/open-buffer-file-mac ()
  "Open current buffer file using Mac `open' command."
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(defun jib/zoxide-wrapper (&optional arg)
  "Use zoxide to open directory in either Finder (default) or Dired (C-u)."
  (interactive "P")
  (when (called-interactively-p 'interactive)
	(let ((input (completing-read "z: " nil)))
	  (if (equal '(4) arg)
		  (dired (shell-command (concat "z " "cs;" " pwd"))) ;; not currently working (shell-command outputs to a buffer)
		(shell-command (concat "z " input "; open ."))))))

;; Automatically tangle our Emacs.org config file when we save it
;; from emacs-from-scratch
(defun jib/org-babel-tangle-config ()
  (interactive)
  (when (string-equal (buffer-file-name)
                      jib/init.org)
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;;;;;;;;;;;;;
;; Utility ;;
;;;;;;;;;;;;;
;; https://emacsredux.com/blog/2013/03/28/google/
(defun jib/er-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
						   (buffer-substring (region-beginning) (region-end))
						 (read-string "Google: "))))))

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2010-07/msg00291.html
(defun jib/www-get-page-title (url)
  (let ((title))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (setq title (match-string 1))
      (goto-char (point-min))
      (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
      (decode-coding-string title (intern (match-string 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Editing/Text Automation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uses simpleclip
(defun jib/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (save-excursion
	(mark-whole-buffer)
	(simpleclip-copy (point-min) (point-max))
	(deactivate-mark))
  (message "Copied entire buffer to clipboard"))

(defun jib/emacs-clipboard-to-system-clipboard ()
  "Set system clipboard to contents of Emacs kill ring."
  (interactive)
  (simpleclip-set-contents (substring-no-properties (nth 0 kill-ring))))

(defun jib/system-clipboard-to-emacs-clipboard ()
  "Set Emacs kill ring to contents of system clipboard."
  (interactive)
  (kill-new (simpleclip-get-contents)))

(defun jib/split-and-close-sentence ()
  "Deletes current word, inserts a period, and capitalizes the next word -
splits the sentence."
  (interactive)
  (kill-word 1)
  (delete-backward-char 1)
  (insert ".")
  (capitalize-word 1))

(defun jib/listify (&optional count)
  "Turn the region's (or count = n lines) into an orgmode list by prepending a +."
  (interactive "p")
  (let ((lines (count-lines (region-beginning) (region-end)))) ;; By default grab a region
	(if (> count 1) (setq lines count)) ;; but if there was an argument, override the # of lines
	(save-excursion
	  (if (use-region-p) ;; If there's a region go to the start and deactivate the region
		  (goto-char (region-beginning)) (deactivate-mark))
	  (while (> lines 0) ;; Add "+ " at the beginning of each line
		(beginning-of-line)
		(insert "+ ")
		(forward-line)
		(setq lines (1- lines))))))

(defun jib/insert-empty-lines-after-each-line ()
  "Add a blank line after each line in a region."
  (interactive)
  (let ((lines (count-lines (region-beginning) (region-end)))) ;; By default grab a region
	(save-excursion
	  (if (use-region-p) ;; If there's a region go to the start and deactivate the region
		  (goto-char (region-beginning)) (deactivate-mark))
	  (while (> lines 0)
		(end-of-line)
		(open-line 1)
		(next-line 2)
		(setq lines (1- lines))))))

(defun jib/pages-from-page-range ()
  "Select a page range formatted as: start-end (e.g. 520-614). Calculates and inserts page count."
  (interactive)
  (if (use-region-p)
	  (let*
		  ((range      (buffer-substring (region-beginning) (region-end)))
		   (dash       (string-match "-" range))
		   (beginning  (string-to-number (substring range 0 dash)))
		   (end        (string-to-number (substring range (+ dash 1) nil)))
		   (difference (+ (- end beginning) 1))) ;; Inclusive (so +1)
		(goto-char (region-end))
		(deactivate-mark)
		(insert " (" (number-to-string difference) ")"))
	(user-error "Error: select a region")))

;;;;;;;;;;;;;;;;;;
;; Calculations ;;
;;;;;;;;;;;;;;;;;;

(defun jib/calc-speaking-time ()
  "Calculate how long it would take me to speak aloud the selection."
  (interactive)
  (if (use-region-p) (let* ((wpm 150)
							(word-count (float (count-words-region (region-beginning) (region-end))))
							(raw-time (* 60 (/ word-count wpm))))
					   (message "%s minutes, %s seconds to speak at %d wpm (%d words)"
								(format-seconds "%m" raw-time)
								(floor (mod raw-time 60)) wpm word-count))
	(error "Error: select a region.")))

(defun jib/time-difference ()
  "Ask for two times/date using `org-read-date' and compute the difference."
  (interactive)
  (message "%s" (ts-human-format-duration ;; Multiply by -1 so first input can be the earlier time
				 (* -1 (ts-difference (ts-parse-org (org-read-date))
									  (ts-parse-org (org-read-date)))))))

(defun jib/return-week-number ()
  (interactive)
  (message "It is week %s of the year." (format-time-string "%U")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Packages Better ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jib/fzf ()
  "Allows you to select a folder to fzf"
  (interactive)
  (let ((current-prefix-arg '-)) ;; emulate C-u
    (call-interactively 'counsel-fzf)))

(defun jib/rg ()
  "Allows you to select a folder to ripgrep."
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'counsel-rg)))

(defun spacemacs/deft ()
  "Helper to call deft and then fix things so that it is nice and works"
  (interactive)
  (deft)
  ;; Hungry delete wrecks deft's DEL override
  (when (fboundp 'hungry-delete-mode)
    (hungry-delete-mode -1))
  ;; When opening it you always want to filter right away
  (evil-insert-state nil))

(defun jib/load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))


;;;;;;;;;;;;;
;; Orgmode ;;
;;;;;;;;;;;;;

(defun jib/org-agenda-caller (letter)
  "Calls a specific org agenda view specified by the letter argument."
  (org-agenda nil letter))

(defun jib/org-copy-link-to-clipboard ()
  "Copy orgmode link to clipboard (simpleclip)."
  (interactive)
  (let ((context (org-element-context)))
	(if (eq (org-element-type context) 'link)
		(simpleclip-set-contents
		 (org-element-property :raw-link context))
	  (user-error "Not a link"))))

(defun jib/org-temp-export-html (&optional arg)
  "Quick, temporary HTML export of org file.
If region is active, export region. Otherwise, export entire file.
If run with universal argument C-u, insert org options to make export very plain."
  (interactive "P")
  (save-window-excursion
	(if (not (use-region-p)) ;; If there is no region active, mark the whole buffer
		(mark-whole-buffer))
	(let ((old-buffer (current-buffer)) (beg (region-beginning)) (end (region-end)))
	  (with-temp-buffer
		(when (equal '(4) arg)
		  (insert "#+OPTIONS: toc:nil date:nil author:nil num:nil title:nil tags:nil \
              	  todo:nil html-link-use-abs-url:nil html-postamble:nil html-preamble:nil html-scripts:nil tex:nil \
                   html-style:nil html5-fancy:nil tex:nil")) ;; If desired, insert these options for a plain export
		(insert "\n \n")
		(insert-buffer-substring old-buffer beg end) ;; Insert desired text to export into temp buffer
		(org-html-export-as-html) ;; Export to HTML
		(write-file (concat (make-temp-file "jibemacsorg") ".html")) ;; Write HTML to temp file
		(jib/open-buffer-file-mac) ;; Use my custom function to open the file (Mac only)
		(kill-this-buffer)))))

(defun jib/org-export-markdown-to-clipboard (&optional arg)
  "Copy org to clipboard as markdown.
If region is active, use region. Otherwise, use entire file."
  (interactive "P")
  (save-window-excursion
	(if (not (use-region-p)) ;; If there is no region active, mark the whole buffer
		(mark-whole-buffer))
	(let ((old-buffer (current-buffer)) (beg (region-beginning)) (end (region-end)))
	  (with-temp-buffer
		(unless (equal '(4) arg) (insert "#+OPTIONS: toc:nil \n")) ;; Unless run with C-u, don't make a TOC
		(insert-buffer-substring old-buffer beg end) ;; Insert desired text to export into temp buffer
		(org-md-export-as-markdown) ;; Export to markdown buffer
		(jib/copy-whole-buffer-to-clipboard)
		(kill-this-buffer)))) ;; Kill the markdown buffer
  (deactivate-mark))

;; WIP
(defun jib/org-temp-export-pdf (&optional arg)
  (interactive "P")
  (let* ((use-title (y-or-n-p "Use a title?"))
		 (use-header (if (eq use-title t)
						 (y-or-n-p "Use section header as title?")))
		 (org-section-title (nth 4 (org-heading-components)))
		 (style (completing-read "Style" '("quicksport" "basic-notes")))
		 (inputted-title (if (and (eq use-title t) (eq use-header nil))
							 (read-string "Title? ")))
		 ) ;; end let* variables section
	(if (use-region-p) 
		(kill-ring-save (region-beginning) (region-end)) ;; If there is a region copy it and use that
	  (org-copy-subtree)) ;; Else copy the subtree
	(save-window-excursion
	  ;; Maybe better to do this with with-temp-buffer. Not sure.
	  (find-file (make-temp-file
				  (concat "jb_" (format-time-string "%m-%d_%I%M-%S") "_") nil ".org"))

	  (org-mode)
	  (yank)
	  (goto-char (point-min))
	  (beginning-of-line)
	  (open-line 1)
	  
	  (yas-expand-snippet (yas-lookup-snippet "LaTeX Setupfile"))

	  ;; Setting export style
	  (cond
	   ((equal style "quicksport") (insert "/jake-latex-quicksport.setup \n")) 
	   ((equal style "basic-notes") (insert "/jake-latex-basic-notes.setup \n#+OPTIONS: toc:nil \n")) 
	   (t (insert ""))
	   )

	  (insert "\n#+DATE: \\today \n")

	  (if (eq use-title t)
		  (cond
		   ((eq use-header t)
			(insert "#+TITLE: " org-section-title)
			(org-next-visible-heading 1)
			(set-mark-command nil)
			(ap/org-forward-to-entry-content)
			(delete-region (region-beginning) (region-end))
			) ;; if this, delete the top level heading and its properties drawer (so the first suhead becomes the first section head)
		   ((eq use-header nil) (insert "#+TITLE: " inputted-title)) ;; If you want a title that's not the section header, just use what was input
		   (t (insert ""))
		   )
		)

	  (start-process "default-app" nil "open" (org-latex-export-to-pdf))
	  (save-buffer)
	  (kill-this-buffer)
	  )
	)
  )

(defun jib/org-schedule-tomorrow ()
  "Org Schedule for tomorrow (+1d)."
  (interactive)
  (org-schedule t "+1d"))

(defun jib/org-set-startup-visibility ()
  "Allows `org-set-startup-visibility' to be used interactively. (it's not an interactive function)"
  (interactive)
  (org-set-startup-visibility)
  (goto-char (point-min)))

(defun jib/org-refile-this-file ()
  "Org refile to only headers in current file, 5 levels."
  (interactive)
  (let ((org-refile-targets '((nil . (:maxlevel . 5)))))
	(org-refile)))

(defun jib/refresh-org-agenda-from-afar ()
  "Refresh org agenda from anywhere."
  (interactive)
  (if (get-buffer "*Org Agenda*")
	  (save-window-excursion
		(switch-to-buffer "*Org Agenda*")
		(org-agenda-redo))))

;; Modified from https://stackoverflow.com/questions/25930097/emacs-org-mode-quickly-mark-todo-as-done?rq=1
(defun jib/org-done-keep-todo ()
  "Mark an org todo item as done while keeping its former keyword intact, and archive.

For example, * TODO This item    becomes    * DONE TODO This item."
  (interactive)
  (let ((state (org-get-todo-state)) (tag (org-get-tags)) (todo (org-entry-get (point) "TODO"))
        post-command-hook)
    (if (not (eq state nil))
        (progn (org-back-to-heading)
			   (org-todo "DONE")
			   (org-set-tags tag)
			   (beginning-of-line)
			   (forward-word)
			   (insert (concat " " todo)))
	  (user-error "Not a TODO."))
    (run-hooks 'post-command-hook)))

(defun jib/org-done-keep-todo-and-archive ()
  "Same as `jib/org-done-keep-todo' but archives heading as well."
  (interactive)
  (let ((state (org-get-todo-state)) (tag (org-get-tags)) (todo (org-entry-get (point) "TODO"))
        post-command-hook)
    (if (not (eq state nil))
        (progn (org-back-to-heading)
			   (org-todo "DONE")
			   (org-set-tags tag)
			   (beginning-of-line)
			   (forward-word)
			   (insert (concat " " todo))
			   (org-archive-subtree-default))
	  (user-error "Not a TODO."))
    (run-hooks 'post-command-hook)))

(defun jib/org-archive-ql-search ()
  "Input or select a tag to search in my archive files."
  (interactive)
  (let* ((choices '("bv" "sp" "ch" "cl" "es" "Robotics ec" "Weekly ec")) ;; TODO get these with org-current-tag-alist
		 (tag (completing-read "Tag: " choices)))
	(org-ql-search
	  ;; Recursively get all .org_archive files from my archive directory
	  (directory-files-recursively
	   (expand-file-name "org-archive" org-directory) ".org_archive")
	  ;; Has the matching tags (can be a property or just a tag) and is a todo - done or not
	  `(and (or (property "ARCHIVE_ITAGS" ,tag) (tags ,tag)) (or (todo) (done))))))

;; (defun get-tags-from-these-buffers ()
;;   (let* ((files (directory-files-recursively (expand-file-name "org-archive" org-directory) ".org_archive")))

;; 	(cl-loop for file in (files)
;; 			 ()
;; 			 if (buffer-file-name buf)
;; 			 collect buf)

;; 	)

;; )	

;; WIP
;; (defun jib/tag-search-export ()
;;   (interactive)
;;   (save-window-excursion
;; 	(let ((org-agenda-tags-column 0))
;; 	  (org-ql-search "~/Dropbox/org/cpb.org" '(tags "article") :super-groups '((:auto-outline-path)) :buffer "steve")
;; 	  (switch-to-buffer "steve")
;; 	  (read-only-mode 0)
;; 	  (goto-char (point-min))
;; 	  (while (search-forward ":article:" nil t) (replace-match ""))
;; 	  (org-agenda-write "~/Desktop/export.html" nil nil "steve")
;; 	  (kill-buffer "steve")))
;;   )

(defun jib/org-occur-unchecked-boxes (&optional arg)
  "Show unchecked Org Mode checkboxes. Ignore items with a `†' at EOL unless run with C-u."
  (interactive "P")
  (if (equal '(4) arg)
	  (occur "\\[ \\].*†$")
	(occur "\\[ \\].*[^†]$")))

(defmacro spacemacs|org-emphasize (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
          (org-emphasize ,char)))

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
(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17"
  (interactive)
  (let* (
         ($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x )
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "#" $path )
            (let (
                  ( $fpath (substring $path 0 (match-beginning 0)))
                  ( $fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char (point-min))
                    (search-forward $fractPart ))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char (point-min))
                      (forward-line (1- $line-num)))
                  (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
                  (find-file $path ))))))))))

(defun jib/prettify-symbols-setup ()
  ;; checkboxes
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;; (push '("[X]" . "☒" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)

  ;; org-babel
  (push '("#+BEGIN_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+END_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+begin_src" . ?≫) prettify-symbols-alist)
  (push '("#+end_src" . ?≫) prettify-symbols-alist)

  (push '("#+BEGIN_QUOTE" . ?❝) prettify-symbols-alist)
  (push '("#+END_QUOTE" . ?❞) prettify-symbols-alist)

  ;; (push '("#+BEGIN_SRC python" . ) prettify-symbols-alist) ;; This is the Python symbol. Comes up weird for some reason
  (push '("#+RESULTS:" . ?≚ ) prettify-symbols-alist)

  ;; drawers
  (push '(":PROPERTIES:" . ?) prettify-symbols-alist)

  ;; tags
  ;; (push '(":Misc:" . "" ) prettify-symbols-alist)

  (prettify-symbols-mode))


;;;;;;;;;;;;
;; Macros ;;
;;;;;;;;;;;;

;; Converts org mode from current line to bottom to HTML and copies it to the system clipboard
;; uses org-html-convert-region-to-html
(fset 'jib|Brinkley-HTML
	  (kmacro-lambda-form [?V ?G ?y ?  ?f ?n ?  ?h ?M ?O ?p ?V ?G ?, ?H ?  ?x ?C ?  ?b ?d] 0 "%d"))

;; OBSELETE
;; ;; Takes an org mode list and adds bullets one indent in under each item
;; (fset 'jib|Listify
;; 	  (kmacro-lambda-form [?0 ?i ?+ ?\S-  escape ?j] 0 "%d"))

;; Takes a single-leveled org mode list and adds a sub item under each item
(fset 'jib|SubListify
      (kmacro-lambda-form [?A M-return tab S-right escape ?j ?0] 0 "%d"))

(provide 'jib-funcs)
;;; jib-funcs.el ends here
