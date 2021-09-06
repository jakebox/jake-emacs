;;; evil-motion-trainer.el ---  Trains you to use better evil motions. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Martin Baillie

;; Author: Martin Baillie <martin@baillie.id>
;; Keywords: learning
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5"))

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Entering `evil-motion-trainer-mode' makes Emacs drop lazily repeated
;; "hjkl"-based evil motions after a configurable threshold, forcing you to think
;; about a more precise motion.

;;; Code:

;;; The following code learns a lot from the old "annoying-arrows" packages.

(require 'cl-lib)

(defgroup evil-motion-trainer nil  "Evil motion trainer" :group 'evil)

(defvar evil-motion-trainer-mode nil
  "Trains you to use better evil motions.")

(defvar evil-motion-trainer-threshold 8
  "Number of permitted repeated key presses.")

(defvar evil-motion-trainer-super-annoying-mode nil
  "Switches to and shows an annoying message in another buffer.")

(defvar evil-motion-trainer--current-count 2
  "Defaults to two because first two keypresses are registered as one.")

;; Since evil-commands sometimes call themselves recursively (evil-forward-char
;; calls itself 2-3 times, for example) we need to ensure that the user actually
;; pressed the keys for those commands several times. We do this by ensuring
;; that the time between command calls is longer than some threshold. Without
;; this check, 3-4 calls of evil-forward char would be enough to trigger.

(defvar emt--old-time (float-time))
(defvar emt--old-temp nil)
(defun emt--check-enough-time-passed (new-time)
  "Check to see if enough time has passed between NEW-TIME and last call."
  (progn
    (setq emt--old-temp emt--old-time)
    (setq emt--old-time new-time)
    (< 0.01 (- emt--old-time emt--old-temp))))

(defvar emt--commands nil)
(defun emt--commands-with-shortcuts (cmds)
  "The subset of CMDS with shortcuts."
  (cl-remove-if (lambda (cmd)
                  (and
                   (>= (length (substitute-command-keys (format "\\[%S]" cmd))) 3)
                   (string-equal
                    (substring (substitute-command-keys (format "\\[%S]" cmd)) 0 3)
                    "M-x"))) cmds))

(defun emt--maybe-block-motion (orig-fn &rest args)
  "Advises whether the evil motion ORIG-FN called with ARGS should be dropped."
  (let ((cmd this-command))
    (when (and evil-motion-trainer-mode
               (emt--check-enough-time-passed (float-time)))
      (if (and (memq this-command emt--commands)
               (or (eq this-command last-command)
                   (eq (get cmd 'alternative-cmd) last-command)))
          (progn
            (cl-incf evil-motion-trainer--current-count)
            (when (> evil-motion-trainer--current-count evil-motion-trainer-threshold)
              (let* ((alts (emt--commands-with-shortcuts (get cmd 'emt--alts)))
                     (alt (nth (random (length alts)) alts))
                     (key (substitute-command-keys (format "\\[%S]" alt)))
                     (msg (format "%s How about using %s (%s) instead?"
                                  (propertize "Lazy motion!" 'face 'bold)
                                  (propertize (format "%S" alt) 'face 'font-lock-type-face)
                                  (propertize (format "%s" key) 'face 'font-lock-keyword-face))))
                (if evil-motion-trainer-super-annoying-mode
                    (progn (switch-to-buffer (get-buffer-create "Evil motion trainer"))
                           (insert msg))
                  (message msg)))))
        (setq evil-motion-trainer--current-count 2)))
    (when (< evil-motion-trainer--current-count evil-motion-trainer-threshold)
      (apply orig-fn args))))

(defmacro add-emt-advice (cmd alternatives &optional native-cmd)
  "Add advice around the provided CMD motion using ALTERNATIVES for suggestions.
The optional NATIVE-CMD is also considered in motion counting."
  `(progn
     (add-to-list 'emt--commands (quote ,cmd))
     (put (quote ,cmd) 'emt--alts ,alternatives)
     (put (quote ,cmd) 'alternative-cmd (quote ,native-cmd))
     (put (quote ,native-cmd) 'alternative-cmd (quote ,cmd))
     (advice-add (quote ,cmd) :around #'emt--maybe-block-motion)))

;; Add some default evil motion training advice.
(add-emt-advice evil-next-line
                '(evil-search-forward evil-jumper/backward evil-snipe-s)
                next-line)
(add-emt-advice evil-next-visual-line
                '(evil-search-forward evil-jumper/backward evil-snipe-s)
                next-line)
(add-emt-advice evil-previous-line
                '(evil-search-backward evil-snipe-S evil-jumper/backward evil-find-char-backward)
                previous-line)
(add-emt-advice evil-previous-visual-line
                '(evil-search-backward evil-snipe-S evil-jumper/backward evil-find-char-backward)
                previous-line)
(add-emt-advice evil-forward-char
                '(evil-search-forward evil-find-char evil-snipe-f evil-snipe-s))
(add-emt-advice evil-backward-char
                '(evil-search-backward evil-find-char-backward evil-snipe-F evil-snipe-S))

;;;###autoload
(define-minor-mode evil-motion-trainer-mode "Evil motion trainer minor mode.")

;;;###autoload
(define-globalized-minor-mode
  global-evil-motion-trainer-mode evil-motion-trainer-mode evil-motion-trainer-mode)

(defun emt-add-suggestion (cmd alternative)
  "Add ALTERNATIVE as a suggestion for CMD."
  (let ((old-alts (or (get cmd 'emt--alts)
                      ())))
    (unless (memq alternative old-alts)
      (put cmd 'emt--alts (cons alternative old-alts)))))

(defun emt-add-suggestions (cmd alternatives)
  "Add ALTERNATIVES as suggestions to CMD."
  (let ((old-alts (or (get cmd 'emt--alts)
                      ())))
    (put cmd 'emt--alts (append (cl-remove-if
                                 (lambda (cmd) (memq cmd old-alts)) alternatives) old-alts))))

(provide 'evil-motion-trainer)
;;; evil-motion-trainer.el ends here
