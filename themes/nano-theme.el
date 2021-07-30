;;; nano-theme.el --- A theme split from nano-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 LiuBo

;; Author: LiuBo <https://github.com/404cn>
;; Created: May 30, 2021
;; Version: 1.0.0
;; Keywords: theme
;; Homepage: https://github.com/404cn/nano-theme.el
;; Package-Requires: ((emacs "28.0.50"))

;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;;; A theme split from nano-emacs.

;;; Code:

(deftheme nano "Theme split from nano-emacs")

(defgroup nano-theme nil
  "Options of nano theme."
  :group 'faces)

(defcustom nano-theme-light/dark 'light
  "Nano theme uses light theme or dark theme?"
  :type 'symbol
  :group 'nano-theme)

(defcustom nano-theme-comment-italic nil
  "Enable italics for comments."
  :type 'boolean
  :group 'nano-theme)

(defcustom nano-theme-keyword-italic nil
  "Enable italics for keywords."
  :type 'boolean
  :group 'nano-theme)

(defun nano-theme--light?dark (light dark)
  "Determine using the LIGHT or the DARK color of nano-theme."
  (if (eq nano-theme-light/dark 'light)
      light
    dark))
(defalias '--l?d #'nano-theme--light?dark)

(let ((foreground (--l?d "#37474F" "#ECEFF4"))
      (background (--l?d "#FFFFFF" "#2E3440"))
      (highlight  (--l?d "#FAFAFA" "#3B4252"))
      (critical   (--l?d "#FF6F00" "#EBCB8B"))
      (salient    (--l?d "#673AB7" "#81A1C1"))
      (strong     (--l?d "#000000" "#ECEFF4"))
      (popout     (--l?d "#FFAB91" "#D08770"))
      (subtle     (--l?d "#ECEFF1" "#434C5E"))
      (faded      (--l?d "#B0BEC5" "#677691")))
  (custom-theme-set-faces
   `nano
   ;; Basic
   `(default                              ((t (:foreground ,foreground :background ,background))))
   `(cursor                               ((t (:background ,foreground))))
   `(fringe                               ((t (:foreground ,faded))))
   `(show-paren-match                     ((t (:foreground ,popout))))
   `(hl-line                              ((t (:background ,highlight))))
   `(highlight                            ((t (:background ,subtle))))
   `(lazy-highlight                       ((t (:background ,subtle))))
   `(region                               ((t (:background ,subtle))))
   `(line-number                          ((t (:background nil :foreground ,faded))))
   `(line-number-current-line             ((t (:background nil :foreground ,strong))))
   `(minibuffer-prompt                    ((t (:foreground ,popout))))
   `(vertical-border                      ((t (:foreground ,subtle))))
   `(window-divider                       ((t (:foreground ,subtle))))
   `(window-divider-first-pixel           ((t (:foreground ,subtle))))
   `(window-divider-last-pixel            ((t (:foreground ,subtle))))
   `(fill-column-indicator                ((t (:foreground ,strong))))
   `(shadow                               ((t (:foreground ,faded))))
   `(success                              ((t (:foreground ,faded))))
   `(warning                              ((t (:foreground ,popout))))
   `(error                                ((t (:foreground ,critical))))
   `(match                                ((t (:foreground ,popout))))
   `(link                                 ((t (:foreground ,salient))))
   `(trailing-whitespace                  ((t (:background ,subtle))))
   `(completions-common-part              ((t (:foreground ,faded))))
   `(secondary-selection                  ((t (:background ,subtle))))

   ;; Font Locks
   `(font-lock-comment-face               ((t (:foreground ,faded :weight bold :slant ,(if nano-theme-comment-italic 'italic 'normal)))))
   `(font-lock-comment-delimiter-face     ((t (:foreground ,faded :weight bold :slant ,(if nano-theme-comment-italic 'italic 'normal)))))
   `(font-lock-keyword-face               ((t (:foreground ,salient :weight bold :slant ,(if nano-theme-keyword-italic 'italic 'normal)))))
   `(font-lock-string-face                ((t (:foreground ,popout))))
   `(font-lock-doc-face                   ((t (:foreground ,faded :extend t))))
   `(font-lock-builtin-face               ((t (:foreground ,salient))))
   `(font-lock-type-face                  ((t (:foreground ,salient :weight bold))))
   `(font-lock-variable-name-face         ((t (:foreground ,strong))))
   `(font-lock-constant-face              ((t (:foreground ,salient :weight bold))))
   `(font-lock-function-name-face         ((t (:foreground ,strong))))
   `(font-lock-warning-face               ((t (:foreground ,popout :weight bold))))

   ;; Eldoc
   `(eldoc-highlight-function-argument    ((t (:foreground ,critical :bold t))))

   ;; ISearch
   `(isearch                              ((t (:foreground ,strong))))
   `(isearch-fail                         ((t (:foreground ,faded))))

   ;; Info
   `(info-menu-header                     ((t (:foreground ,foreground :bold t))))
   `(info-header-node                     ((t (:foreground ,foreground :background ,background))))
   `(info-index-match                     ((t (:foreground ,salient))))
   `(Info-quoted                          ((t (:foreground ,faded))))
   `(info-title-1                         ((t (:inhert info-menu-header))))
   `(info-title-2                         ((t (:inhert info-menu-header))))
   `(info-title-3                         ((t (:inhert info-menu-header))))
   `(info-title-4                         ((t (:inhert info-menu-header))))

   ;; Speed Bar
   `(speedbar-button-face                 ((t (:foreground ,faded))))
   `(speedbar-directory-face              ((t (:foreground ,foreground :bold t))))
   `(speedbar-file-face                   ((t (:foreground ,foreground :background ,background))))
   `(speedbar-highlight-face              ((t (:foreground ,highlight))))
   `(speedbar-selected-face               ((t (:background ,subtle))))
   `(speedbar-separator-face              ((t (:foreground ,faded))))
   `(speedbar-tag-face                    ((t (:foreground ,faded))))

   ;; Bookmark
   `(bookmark-menu-heading                ((t (:foreground ,foreground :bold t))))
   `(bookmark-menu-bookmark               ((t (:foreground ,salient))))

   ;; Outline
   `(outline-1                            ((t (:foreground ,foreground :bold t))))
   `(outline-2                            ((t (:foreground ,foreground :bold t))))
   `(outline-3                            ((t (:foreground ,foreground :bold t))))
   `(outline-4                            ((t (:foreground ,foreground :bold t))))
   `(outline-5                            ((t (:foreground ,foreground :bold t))))
   `(outline-6                            ((t (:foreground ,foreground :bold t))))
   `(outline-7                            ((t (:foreground ,foreground :bold t))))
   `(outline-8                            ((t (:foreground ,foreground :bold t))))

   ;; Message
   `(message-cited-text                   ((t (:foreground ,faded))))
   `(message-header-cc                    ((t (:foreground ,foreground :background ,background))))
   `(message-header-name                  ((t (:foreground ,foreground :bold t))))
   `(message-header-newsgroups            ((t (:foreground ,foreground :background ,background))))
   `(message-header-other                 ((t (:foreground ,foreground :background ,background))))
   `(message-header-subject               ((t (:foreground ,salient))))
   `(message-header-to                    ((t (:foreground ,salient))))
   `(message-header-xheader               ((t (:foreground ,foreground :background ,background))))
   `(message-mml                          ((t (:foreground ,popout))))
   `(message-separator                    ((t (:foreground ,faded))))

   ;; Customize
   `(widget-field                         ((t (:background ,subtle))))
   `(widget-button                        ((t (:foreground ,foreground :bold t))))
   `(widget-single-line-field             ((t (:background ,subtle))))
   `(custom-group-subtitle                ((t (:foreground ,foreground :bold t))))
   `(custom-group-tag                     ((t (:foreground ,foreground :bold t))))
   `(custom-group-tag-1                   ((t (:foreground ,foreground :bold t))))
   `(custom-comment                       ((t (:foreground ,faded))))
   `(custom-comment-tag                   ((t (:foreground ,faded))))
   `(custom-changed                       ((t (:foreground ,salient))))
   `(custom-modified                      ((t (:foreground ,salient))))
   `(custom-face-tag                      ((t (:foreground ,foreground :bold t))))
   `(custom-variable-tag                  ((t (:foreground ,foreground :bold t))))
   `(custom-invalid                       ((t (:foreground ,popout))))
   `(custom-visibility                    ((t (:foreground ,salient))))
   `(custom-state                         ((t (:foreground ,salient))))
   `(custom-link                          ((t (:foreground ,salient))))
   `(custom-button                        ((t (:foreground ,faded :background ,background :box `(:line-width 1 :color ,(face-foreground 'faded) :style nil)))))
   `(custom-button-mouse                  ((t (:foreground ,faded :background ,subtle :box `(:line-width 1 :color ,(face-foreground 'faded) :style nil)))))
   `(custom-button-pressed                ((t (:foreground ,foreground :background ,salient :inverse-video nil :box `(:line-width 1 :color ,(face-foreground 'salient) :style nil)))))

   ;; Package
   `(package-description                  ((t (:foreground ,foreground :background ,background))))
   `(package-help-section-name            ((t (t (:foreground ,foreground :background ,background)))))
   `(package-name                         ((t (:foreground ,salient))))
   `(package-status-avail-obso            ((t (:foreground ,faded))))
   `(package-status-available             ((t (:foreground ,foreground :background ,background))))
   `(package-status-built-in              ((t (:foreground ,salient))))
   `(package-status-dependency            ((t (t (:foreground ,salient)))))
   `(package-status-disabled              ((t (:foreground ,faded))))
   `(package-status-external              ((t (:foreground ,foreground :background ,background))))
   `(package-status-held                  ((t (:foreground ,foreground :background ,background))))
   `(package-status-incompat              ((t (:foreground ,faded))))
   `(package-status-installed             ((t (:foreground ,salient))))
   `(package-status-new                   ((t (:foreground ,foreground :background ,background))))
   `(package-status-new                   ((t (:foreground ,foreground :background ,background))))

   ;; Flyspell
   `(flyspell-duplicate                   ((t (:foreground ,popout))))
   `(flyspell-incorrect                   ((t (:foreground ,popout))))

   ;; Ido
   `(ido-first-match                      ((t (:foreground ,salient))))
   `(ido-only-match                       ((t (:foreground ,faded))))
   `(ido-subdir                           ((t (:foreground ,foreground :bold t))))

   ;; Diff
   `(diff-header                          ((t (:foreground ,faded))))
   `(diff-file-header                     ((t (:foreground ,foreground :bold t))))
   `(diff-context                         ((t (:foreground ,foreground :background ,background))))
   `(diff-removed                         ((t (:foreground ,faded))))
   `(diff-changed                         ((t (:foreground ,popout))))
   `(diff-added                           ((t (:foreground ,salient))))
   `(diff-refine-added                    ((t (:foreground ,salient :bold t))))
   `(diff-refine-changed                  ((t (:foreground ,popout))))
   `(diff-refine-removed                  ((t (:foreground ,faded :strike-through t))))

   ;; Agenda
   `(org-agenda-calendar-event            ((t (:foreground ,foreground :background ,background))))
   `(org-agenda-calendar-sexp             ((t (:foreground ,salient))))
   `(org-agenda-clocking                  ((t (:foreground ,faded))))
   `(org-agenda-column-dateline           ((t (:foreground ,faded))))
   `(org-agenda-current-time              ((t (:foreground ,foreground :bold t))))
   `(org-agenda-date                      ((t (:foreground ,salient))))
   `(org-agenda-date-today                ((t (:foreground ,salient :bold t))))
   `(org-agenda-date-weekend              ((t (:foreground ,faded))))
   `(org-agenda-diary                     ((t (:foreground ,faded))))
   `(org-agenda-dimmed-todo-face          ((t (:foreground ,faded))))
   `(org-agenda-done                      ((t (:foreground ,faded))))
   `(org-agenda-filter-category           ((t (:foreground ,faded))))
   `(org-agenda-filter-effort             ((t (:foreground ,faded))))
   `(org-agenda-filter-regexp             ((t (:foreground ,faded))))
   `(org-agenda-filter-tags               ((t (:foreground ,faded))))
   `(org-agenda-restriction-lock          ((t (:foreground ,faded))))
   `(org-agenda-structure                 ((t (:foreground ,foreground :bold t))))

   ;; Org
   `(org-archived                         ((t (:foreground ,faded))))
   `(org-block                            ((t (:background ,subtle))))
   `(org-block-begin-line                 ((t (:foreground ,faded))))
   `(org-block-end-line                   ((t (:foreground ,faded))))
   `(org-checkbox                         ((t (:foreground ,faded))))
   `(org-checkbox-statistics-done         ((t (:foreground ,faded))))
   `(org-checkbox-statistics-todo         ((t (:foreground ,faded))))
   `(org-clock-overlay                    ((t (:foreground ,faded))))
   `(org-code                             ((t (:foreground ,faded))))
   `(org-column                           ((t (:foreground ,faded))))
   `(org-column-title                     ((t (:foreground ,faded))))
   `(org-date                             ((t (:foreground ,faded))))
   `(org-date-selected                    ((t (:foreground ,faded))))
   `(org-default                          ((t (:foreground ,faded))))
   `(org-document-info                    ((t (:foreground ,faded))))
   `(org-document-info-keyword            ((t (:foreground ,faded))))
   `(org-document-title                   ((t (:foreground ,foreground :bold t))))
   `(org-done                             ((t (:foreground ,foreground :background ,background))))
   `(org-drawer                           ((t (:foreground ,faded))))
   `(org-ellipsis                         ((t (:foreground ,faded))))
   `(org-footnote                         ((t (:foreground ,faded))))
   `(org-formula                          ((t (:foreground ,faded))))
   `(org-headline-done                    ((t (:foreground ,faded))))
   `(org-latex-and-related                ((t (:foreground ,faded))))
   `(org-level-1                          ((t (:foreground ,foreground :bold t :height 1.1))))
   `(org-level-2                          ((t (:foreground ,foreground :bold t))))
   `(org-level-3                          ((t (:foreground ,foreground :bold nil))))
   `(org-level-4                          ((t (:foreground ,foreground :bold nil))))
   `(org-level-5                          ((t (:foreground ,foreground :bold nil))))
   `(org-level-6                          ((t (:foreground ,foreground :bold nil))))
   `(org-level-7                          ((t (:foreground ,foreground :bold nil))))
   `(org-level-8                          ((t (:foreground ,foreground :bold nil))))
   `(org-link                             ((t (:foreground ,salient))))
   `(org-list-dt                          ((t (:foreground ,faded))))
   `(org-macro                            ((t (:foreground ,faded))))
   `(org-meta-line                        ((t (:foreground ,faded))))
   `(org-mode-line-clock                  ((t (:foreground ,faded))))
   `(org-mode-line-clock-overrun          ((t (:foreground ,faded))))
   `(org-priority                         ((t (:foreground ,faded))))
   `(org-property-value                   ((t (:foreground ,faded))))
   `(org-quote                            ((t (:foreground ,faded))))
   `(org-scheduled                        ((t (:foreground ,faded))))
   `(org-scheduled-previously             ((t (:foreground ,faded))))
   `(org-scheduled-today                  ((t (:foreground ,faded))))
   `(org-sexp-date                        ((t (:foreground ,faded))))
   `(org-special-keyword                  ((t (:foreground ,faded))))
   `(org-table                            ((t (:foreground ,faded))))
   `(org-tag                              ((t (:foreground ,popout))))
   `(org-tag-group                        ((t (:foreground ,faded))))
   `(org-target                           ((t (:foreground ,faded))))
   `(org-time-grid                        ((t (:foreground ,faded))))
   `(org-todo                             ((t (:foreground ,salient))))
   `(org-upcoming-deadline                ((t (:foreground ,foreground :background ,background))))
   `(org-verbatim                         ((t (:foreground ,popout))))
   `(org-verse                            ((t (:foreground ,faded))))
   `(org-warning                          ((t (:foreground ,popout))))

   ;; Elfeed
   `(elfeed-log-date-face                 ((t (:foreground ,faded))))
   `(elfeed-log-info-level-face           ((t (:foreground ,foreground :background ,background))))
   `(elfeed-log-debug-level-face          ((t (:foreground ,foreground :background ,background))))
   `(elfeed-log-warn-level-face           ((t (:foreground ,popout))))
   `(elfeed-log-error-level-face          ((t (:foreground ,popout))))
   `(elfeed-search-tag-face               ((t (:foreground ,faded))))
   `(elfeed-search-date-face              ((t (:foreground ,faded))))
   `(elfeed-search-feed-face              ((t (:foreground ,salient))))
   `(elfeed-search-filter-face            ((t (:foreground ,faded))))
   `(elfeed-search-last-update-face       ((t (:foreground ,salient))))
   `(elfeed-search-title-face             ((t (:foreground ,foreground :background ,background))))
   `(elfeed-search-tag-face               ((t (:foreground ,faded))))
   `(elfeed-search-unread-count-face      ((t (:foreground ,foreground :bold t))))
   `(elfeed-search-unread-title-face      ((t (:foreground ,foreground :bold t))))

   ;; Rst
   `(rst-adornment                        ((t (:foreground ,faded))))
   `(rst-block                            ((t (:foreground ,foreground :background ,background))))
   `(rst-comment                          ((t (:foreground ,faded))))
   `(rst-definition                       ((t (:foreground ,salient))))
   `(rst-directive                        ((t (:foreground ,salient))))
   `(rst-emphasis1                        ((t (:foreground ,faded))))
   `(rst-emphasis2                        ((t (:foreground ,foreground :bold t))))
   `(rst-external                         ((t (:foreground ,salient))))
   `(rst-level-1                          ((t (:foreground ,foreground :bold t))))
   `(rst-level-2                          ((t (:foreground ,foreground :bold t))))
   `(rst-level-3                          ((t (:foreground ,foreground :bold t))))
   `(rst-level-4                          ((t (:foreground ,foreground :bold t))))
   `(rst-level-5                          ((t (:foreground ,foreground :bold t))))
   `(rst-level-6                          ((t (:foreground ,foreground :bold t))))
   `(rst-literal                          ((t (:foreground ,salient))))
   `(rst-reference                        ((t (:foreground ,salient))))
   `(rst-transition                       ((t (:foreground ,foreground :background ,background))))

   ;; Deft
   `(deft-filter-string-error-face        ((t (:foreground ,popout))))
   `(deft-filter-string-face              ((t (:foreground ,foreground :background ,background))))
   `(deft-header-face                     ((t (:foreground ,salient))))
   `(deft-separator-face                  ((t (:foreground ,faded))))
   `(deft-summary-face                    ((t (:foreground ,faded))))
   `(deft-time-face                       ((t (:foreground ,salient))))
   `(deft-title-face                      ((t (:foreground ,foreground :bold t))))

   ;; Markdown
   `(markdown-blockquote-face             ((t (:inhert default))))
   `(markdown-bold-face                   ((t (:foreground ,foreground :bold t))))
   `(markdown-code-face                   ((t (:inhert default))))
   `(markdown-comment-face                ((t (:foreground ,faded))))
   `(markdown-footnote-marker-face        ((t (:inhert default))))
   `(markdown-footnote-text-face          ((t (:inhert default))))
   `(markdown-gfm-checkbox-face           ((t (:inhert default))))
   `(markdown-header-delimiter-face       ((t (:foreground ,faded))))
   `(markdown-header-face                 ((t (:foreground ,foreground :bold t))))
   `(markdown-header-face-1               ((t (:foreground ,foreground :bold t))))
   `(markdown-header-face-2               ((t (:foreground ,foreground :bold t))))
   `(markdown-header-face-3               ((t (:foreground ,foreground :bold t))))
   `(markdown-header-face-4               ((t (:foreground ,foreground :bold t))))
   `(markdown-header-face-5               ((t (:foreground ,foreground :bold t))))
   `(markdown-header-face-6               ((t (:foreground ,foreground :bold t))))
   `(markdown-header-rule-face            ((t (:inhert default))))
   `(markdown-highlight-face              ((t (:inhert default))))
   `(markdown-hr-face                     ((t (:inhert default))))
   `(markdown-html-attr-name-face         ((t (:inhert default))))
   `(markdown-html-attr-value-face        ((t (:inhert default))))
   `(markdown-html-entity-face            ((t (:inhert default))))
   `(markdown-html-tag-delimiter-face     ((t (:inhert default))))
   `(markdown-html-tag-name-face          ((t (:inhert default))))
   `(markdown-inline-code-face            ((t (:foreground ,popout))))
   `(markdown-italic-face                 ((t (:foreground ,faded))))
   `(markdown-language-info-face          ((t (:inhert default))))
   `(markdown-language-keyword-face       ((t (:inhert default))))
   `(markdown-line-break-face             ((t (:inhert default))))
   `(markdown-link-face                   ((t (:foreground ,salient))))
   `(markdown-link-title-face             ((t (:inhert default))))
   `(markdown-list-face                   ((t (:foreground ,faded))))
   `(markdown-markup-face                 ((t (:foreground ,faded))))
   `(markdown-math-face                   ((t (:inhert default))))
   `(markdown-metadata-key-face           ((t (:foreground ,faded))))
   `(markdown-metadata-value-face         ((t (:foreground ,faded))))
   `(markdown-missing-link-face           ((t (:inhert default))))
   `(markdown-plain-url-face              ((t (:inhert default))))
   `(markdown-pre-face                    ((t (:inhert default))))
   `(markdown-reference-face              ((t (:foreground ,salient))))
   `(markdown-strike-through-face         ((t (:foreground ,faded))))
   `(markdown-table-face                  ((t (:inhert default))))
   `(markdown-url-face                    ((t (:foreground ,salient))))

   ;; Mu4e
   `(mu4e-attach-number-face              ((t (:foreground ,foreground :bold t))))
   `(mu4e-cited-1-face                    ((t (:foreground ,faded))))
   `(mu4e-cited-2-face                    ((t (:foreground ,faded))))
   `(mu4e-cited-3-face                    ((t (:foreground ,faded))))
   `(mu4e-cited-4-face                    ((t (:foreground ,faded))))
   `(mu4e-cited-5-face                    ((t (:foreground ,faded))))
   `(mu4e-cited-6-face                    ((t (:foreground ,faded))))
   `(mu4e-cited-7-face                    ((t (:foreground ,faded))))
   `(mu4e-compose-header-face             ((t (:foreground ,faded))))
   `(mu4e-compose-separator-face          ((t (:foreground ,faded))))
   `(mu4e-contact-face                    ((t (:foreground ,salient))))
   `(mu4e-context-face                    ((t (:foreground ,faded))))
   `(mu4e-draft-face                      ((t (:foreground ,faded))))
   `(mu4e-flagged-face                    ((t (:foreground ,popout))))
   `(mu4e-footer-face                     ((t (:foreground ,faded))))
   `(mu4e-forwarded-face                  ((t (:inhert default))))
   `(mu4e-header-face                     ((t (:inhert default))))
   `(mu4e-header-highlight-face           ((t (:inhert hl-line))))
   `(mu4e-header-key-face                 ((t (:foreground ,foreground :bold t))))
   `(mu4e-header-marks-face               ((t (:foreground ,faded))))
   `(mu4e-header-title-face               ((t (:foreground ,foreground :bold t))))
   `(mu4e-header-value-face               ((t (:inhert default))))
   `(mu4e-highlight-face                  ((t (:foreground ,popout))))
   `(mu4e-link-face                       ((t (:foreground ,salient))))
   `(mu4e-modeline-face                   ((t (:foreground ,faded))))
   `(mu4e-moved-face                      ((t (:foreground ,faded))))
   `(mu4e-ok-face                         ((t (:foreground ,faded))))
   `(mu4e-region-code                     ((t (:foreground ,faded))))
   `(mu4e-replied-face                    ((t (:inhert default))))
   `(mu4e-special-header-value-face       ((t (:inhert default))))
   `(mu4e-system-face                     ((t (:foreground ,faded))))
   `(mu4e-title-face                      ((t (:foreground ,foreground :bold t))))
   `(mu4e-trashed-face                    ((t (:foreground ,faded))))
   `(mu4e-unread-face                     ((t (:foreground ,foreground :bold t))))
   `(mu4e-url-number-face                 ((t (:foreground ,faded))))
   `(mu4e-view-body-face                  ((t (:inhert default))))
   `(mu4e-warning-face                    ((t (:foreground ,popout))))

   ;; Company
   `(company-tooltip                      ((t (:background ,subtle :foreground ,foreground))))
   `(company-tooltip-selection            ((t (:background ,popout))))
   `(company-tooltip-annotation           ((t (:foreground ,foreground))))
   `(company-tooltip-annotation-selection ((t (:foreground ,strong :bold t))))
   `(company-tooltip-common               ((t (:foreground ,strong))))
   `(company-tooltip-common-selection     ((t (:foreground ,strong :bold t))))
   `(company-scrollbar-bg                 ((t (:background ,faded))))
   `(company-scrollbar-fg                 ((t (:background ,foreground))))

   ;; Calendar
   `(calendar-today                       ((t (:foreground ,foreground :bold t))))

   ;; Mode Line
   `(mode-line                            ((t (:background ,highlight))))
   `(mode-line-inactive                   ((t (:background ,subtle))))
   `(header-line                          ((t (:background ,highlight))))
   `(header-line-inactive                 ((t (:background ,subtle))))

   ;; Solaire Mode
   `(solaire-default-face                 ((t (:inherit default :background ,highlight))))
   `(solaire-minibuffer-face              ((t (:inherit default :background ,highlight))))

   ;; Orderless
   `(orderless-match-face-0               ((t (:foreground ,popout :bold t))))
   `(orderless-match-face-1               ((t (:foreground ,popout :bold t))))
   `(orderless-match-face-2               ((t (:foreground ,popout :bold t))))
   `(orderless-match-face-3               ((t (:foreground ,popout :bold t))))

   ;;Eshell
   `(eshell-prompt                        ((t (:foreground ,popout :bold t))))

   ;; Meow
   `(meow-keypad-indicator                ((t (:foreground ,background :background ,salient))))
   `(meow-insert-indicator                ((t (:foreground ,background :background ,critical))))
   `(meow-normal-indicator                ((t (:foreground ,background :background ,faded))))
   `(meow-motion-indicator                ((t (:foreground ,background :background ,popout))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'nano)

;;; nano-theme.el ends here
