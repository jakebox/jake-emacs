;; -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------------------- ;;
;; This early-init.el file was auto-tangled from an orgmode file. (C) Jake B        ;;
;; -------------------------------------------------------------------------------- ;;

;; setq gc-cons-threshold most-positive-fixnum)

;; Garbage Collections
(setq gc-cons-percentage 0.6)

;; Gccemacs
;;  (setq comp-async-report-warnings-errors nil)

;; Compile warnings
;;  (setq byte-compile-warnings t)
;;  (setq warning-minimum-level :emergency) ;; temporary

;; Window configuration
(setq frame-inhibit-implied-resize t) ;; Supposed to hasten startup

;; Hides toolbar, scroll bars
(unless (eq window-system 'ns)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; This makes the Aqua titlebar color the same as Emacs. I don't really like it, for some reason, though I feel like I should.
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
