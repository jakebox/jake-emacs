;; -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------------------- ;;
;; This early-init.el file was auto-tangled from an orgmode file. (C) Jake B        ;;
;; -------------------------------------------------------------------------------- ;;

;; Garbage Collections
(setq gc-cons-percentage 0.6)

;; Compile warnings
;;  (setq byte-compile-warnings t)
;;  (setq warning-minimum-level :emergency)
(setq comp-async-report-warnings-errors nil) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))


;; MISC OPTIMIZATIONS ----
;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
                   ;;; optimizations (froom Doom's core.el). See that file for descriptions.
(setq idle-update-delay 1.0)

;; Disabling bidi (bidirectional editing stuff)
;; Disables bi-directional editing (like if I wrote in both Hebrew and English)
(setq-default bidi-display-reordering 'left-to-right 
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)  ; emacs 27 only - disables bidirectional parenthesis

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

(setq mac-command-modifier 'meta
      mac-option-modifier nil
      mac-control-modifier 'control
      mac-right-command-modifier 'super
      mac-right-control-modifier 'hyper)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Window configuration
(setq frame-inhibit-implied-resize t) ;; Supposed to hasten startup

;; Less clutter (this is what dfrosted12 uses so I trust that)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; This makes the Aqua titlebar color the same as Emacs.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
