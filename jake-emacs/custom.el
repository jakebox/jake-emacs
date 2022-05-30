(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-file "/Users/jake/.emacs.default/custom.el")
 '(custom-safe-themes
   '("6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "0568a5426239e65aab5e7c48fa1abde81130a87ddf7f942613bf5e13bf79686b" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "3a0d3f2a02d7bbd1ea19bd242f9a53adf2798c0870a9f437372a28bbc1a278df" "1ca9614f572a10bdbdbfde80dc909790df530ca6e5fee3e270f918321c947fbf" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "a6838a652cdc2f7c4591998db5b5c8d1b872fc49e3985163a3aabc0263d6c2fd" "82dc7736d9cb3940fd995cf4fb5191fc82d737fa9a970952d0c83e666c1ede50" "c256ccc73bc6588b5cf77f0e4c9d9bb80a6ebec19c7841ea21fd920642740f2d" "7546a14373f1f2da6896830e7a73674ef274b3da313f8a2c4a79842e8a93953e" default))
 '(org-agenda-files
   '("/Users/jake/Dropbox/org/cal-sync/jboxerman.org" "/Users/jake/Dropbox/org/cal-sync/extracurri.org" "/Users/jake/Dropbox/org/work.org"))
 '(org-ql-views
   '(("Jake Work View No Scheduled" :buffers-files
	  ("~/Dropbox/org/work.org")
	  :query
	  (and
	   (todo)
	   (not
		(habit)))
	  :title "Jake Work View No Scheduled " :sort
	  (priority date deadline)
	  :super-groups
	  ((:name "Important" :discard
			  (:scheduled future)
			  :and
			  (:todo "TODO" :priority "A")
			  :todo "CONTACT" :order 3)
	   (:name "Short-term Todo" :tag "nf" :order 4)
	   (:name "Someday" :todo "SOMEDAY" :order 30)
	   (:name "Homework" :todo
			  ("HW" "READ")
			  :order 5)
	   (:name "Studying" :todo "STUDYING" :order 7)
	   (:name "Quick Picks" :tag "qp" :order 11)
	   (:name "College" :category "college" :order 35)
	   (:name "Projects" :todo "PROJ" :order 12)
	   (:name "Weekly" :tag "weekly" :order 15)
	   (:name "Extracurricular" :discard
			  (:todo "SOMEDAY")
			  :tag
			  ("robotics" "psych" "ec")
			  :order 13)
	   (:name "Personal" :category "personal" :order 27)
	   (:name "Todo" :discard
			  (:category "personal")
			  :todo "TODO" :order 20))
	  :narrow nil)
	 ("CPB Todos and Ideas " :buffers-files
	  ("~/Dropbox/org/cpb.org")
	  :query
	  (or
	   (tags "todo")
	   (tags "idea"))
	  :title "CPB Todos, Ideas, Projects " :super-groups
	  ((:auto-outline-path))
	  :narrow nil)
	 ("Refile" :buffers-files
	  ("~/Dropbox/org/cpb.org")
	  :query
	  (parent
	   (heading "Refile"))
	  :super-groups
	  ((:auto-outline-path)))
	 ("CPB Articles" :buffers-files "~/Dropbox/org/cpb.org" :query
	  (tags "article")
	  :title "CPB Articles" :super-groups
	  ((:auto-outline-path)))
	 ("Jake Work Full View" :buffers-files
	  ("~/Dropbox/org/work.org" "~/Dropbox/org/cpb.org")
	  :query
	  (and
	   (todo)
	   (not
		(habit)))
	  :title "Jake Work Full View " :sort
	  (priority date deadline)
	  :super-groups
	  ((:name "Important" :discard
			  (:tag "habit")
			  :and
			  (:todo "TODO" :priority "A")
			  :todo "CONTACT" :order 3)
	   (:name "Short-term Todo" :tag "nf" :order 4)
	   (:name "Someday" :todo "SOMEDAY" :order 30)
	   (:name "Homework" :todo
			  ("HW" "READ")
			  :order 5)
	   (:name "Studying" :todo "STUDY" :order 7)
	   (:name "Quick Picks" :tag "qp" :order 11)
	   (:name "College" :category "college" :order 35)
	   (:name "Projects" :todo "PROJ" :order 12)
	   (:name "Weekly" :tag "weekly" :order 15)
	   (:name "Extracurricular" :discard
			  (:todo "SOMEDAY")
			  :tag
			  ("robotics" "psych" "ec")
			  :order 13)
	   (:name "Personal" :category "personal" :order 27)
	   (:name "Todo" :todo "TODO" :order 20))
	  :narrow nil)
	 ("Unscheduled TODOs" :buffers-files
	  ("~/Dropbox/org/work.org")
	  :query
	  (and
	   (todo)
	   (not
		(scheduled)))
	  :title "Unscheduled TODOs" :sort
	  (deadline)
	  :narrow nil)))
 '(package-selected-packages
   '(emacs-lisp-mode pyvenv org-super-agenda visual-fill-column evil popper writeroom-mode wordel windresize which-key web-mode use-package unfill undo-fu svg-lib super-save smartparens simpleclip shrink-path reveal-in-osx-finder restart-emacs rainbow-mode python-mode presentation pdf-tools ox-reveal ox-hugo org-tree-slide org-superstar org-real org-ql org-gcal org-download org-appear org-analyzer mw-thesaurus modus-themes mixed-pitch magit kaolin-themes ivy-prescient ivy-hydra htmlize hl-todo hide-mode-line gnuplot general gcmh flyspell-correct-ivy evil-surround evil-snipe evil-org evil-collection evil-anzu esxml elpy doom-themes diminish deft define-word dashboard counsel company-prescient company-auctex centered-cursor-mode burly bufler auto-virtualenv all-the-icons-ivy-rich ace-window))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(safe-local-variable-values
   '((org-image-actual-width . 500)
	 (org-cycle-include-plain-lists . integrate)))
 '(warning-suppress-types
   '((comp)
	 ((defvaralias losing-value modus-themes-slanted-constructs))
	 (yasnippet backquote-change))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:family "Roboto Mono"))))
 '(mode-line ((t (:height 135))))
 '(mode-line-inactive ((t (:height 135))))
 '(org-drawer ((t (:height 0.6 :inherit shadow))))
 '(org-ellipsis ((t (:foreground unspecified :inherit 'shadow))))
 '(org-level-3 ((t (:inherit outline-3 :weight regular))))
 '(org-level-4 ((t (:inherit outline-4 :weight regular))))
 '(org-level-5 ((t (:inherit outline-5 :weight regular))))
 '(org-link ((t (:inherit link :weight regular))))
 '(org-warning ((t (:underline nil)))))
