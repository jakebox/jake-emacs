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

;; Window configuration
(setq frame-inhibit-implied-resize t) ;; Supposed to hasten startup

;; Hides toolbar, scroll bars
;; (unless (eq window-system 'ns)
;;   (tool-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))
;; (when (fboundp 'horizontal-scroll-bar-mode)
;;   (horizontal-scroll-bar-mode -1))

;; Less clutter (this is what dfrosted12 uses so I trust that)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; This makes the Aqua titlebar color the same as Emacs. I don't really like it, for some reason, though I feel like I should.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
