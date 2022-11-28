;;; init.el --- Init file for MMacs -*- lexical-binding: t; -*-
;; Author:         MountainMan1312

;; ------------------------------------------------------------------------
;;; COMMENTARY
;; ------------------------------------------------------------------------

;; ------------------------------------
;; Configuration File Structure
;; ----------------------------

;; COMMENTARY
;; STARTUP
;; PACKAGE MANAGEMENT
;; PERFORMANCE
;; UI SETTINGS
;; EXWM
;; EDITOR
;; SEARCH & COMPLETION
;; FILE & PROJECT MANAGEMENT
;; SYSTEM ADMINISTRATION
;; PROGRAMMING
;; PKMS
;; COMMUNICATIONS
;; LAYOUT AUTOMATION
;; KEYBINDS & CONTROLS
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; STARTUP
;; ------------------------------------------------------------------------

;; ------------------------------------
;; EMACS SERVER
;; ------------
(server-start)
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; PACKAGE MANAGEMENT
;; ------------------------------------------------------------------------

;; ------------------------------------
;; DON'T LOAD OUTDATED CODE
;; ------------------------
(setq load-prefer-newer t)
;; ------------------------------------

;; ------------------------------------
;; STRAIGHT.EL
;; -----------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; ------------------------------------

;; ------------------------------------
;; USE-PACKAGE
;; -----------
(straight-use-package 'use-package)
;; ------------------------------------

;; ------------------------------------
;; Custom vars file
;; ----------------
;; Those custom variables get annoying after a while.
;; Make them go away.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
;; ------------------------------------

;; ------------------------------------
;; AUTOMATIC UPDATES
;; -----------------
(use-package auto-package-update
  :straight t
  :init
  (auto-package-update-maybe)
  (setq auto-package-update-hide-results t))
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; PERFORMANCE
;; ------------------------------------------------------------------------

;; ------------------------------------
;; GARBAGE COLLECTION
;; ------------------
(use-package gcmh
  :straight t)
;; ------------------------------------

;; ------------------------------------
;; ASYNC
;; -----
;; Emacs is single-threaded by default.
;; This package adds some asynchronous processing capability.
(use-package async
  :straight t)

;; This enables asychronous copying, renaming, moving, etc. with Helm.
(dired-async-mode 1)

;; Compile emacs packages asynchronously on startup.
(async-bytecomp-package-mode 1)
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; UI SETTINGS
;; ------------------------------------------------------------------------

;; ------------------------------------
;; Disable mouse-centric UI elements
;; ---------------------------------
;; Emacs has a bunch of unneccessary (for me) stuff
;; in the UI by default. It's "mouse-centric" and
;; takes up spece for no benefit to me.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode -1)
(setq use-dialog-box nil)
(setq inhibit-startup-message t
      initial-scratch-message "")
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; EXWM
;; ------------------------------------------------------------------------




;; ------------------------------------------------------------------------
;;; EDITOR
;; ------------------------------------------------------------------------

;; ------------------------------------
;; Scrolling
;; ---------
;; Why does Emacs scroll at the speed of light?
(setq scroll-conservatively 100
      mouse-wheel-scroll-amount '(2((shift) . 2))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      scroll-margin 0
      auto-window-vscroll nil)

;; PgUp / PgDown scroll to top/bottom line
(setq scroll-error-top-bottom t)

;; Visual-only scroll bar
(use-package yascroll
  :straight t)
(global-yascroll-bar-mode 1)
;; ------------------------------------

;; ------------------------------------
;; Whitespace
;; ----------
;; Show stray whitespace
(setq-default show-trailing-whitespace t
	            indicate-empty-lines t
	            indicate-buffer-boundaries 'left)

;; Make files end with a newline
(setq-default require-final-newline t)

;; Consider a period followed by a single
;; space to be the end of a sentence.
(setq-default sentence-end-double-space nil)

;; Use spaces for indentation
(setq-default indent-tabs-mode nil
	            tab-width 2)

;; Keep code always indented
(use-package aggressive-indent
  :straight t)
(global-aggressive-indent-mode)

;; Indentation alignment guides
(use-package indent-guide
  :straight t
  :ensure t
  :config
  (setq indent-guide-delay 0
	      indent-guide-char "|"
	      indent-guide-recursive nil))
(add-hook 'prog-mode-hook 'indent-guide-mode)
;; ------------------------------------

;; ------------------------------------
;; Line & Column Numbers
;; ---------------------
(column-number-mode)
(global-display-line-numbers-mode t)
;; ------------------------------------

;; ------------------------------------
;; Selection
;; ---------
;; Overwrite selected text
(delete-selection-mode t)

;; Better sub-word selection
(global-subword-mode 1)
;; ------------------------------------

;; ------------------------------------
;; Delimiters
;; ----------
;; Auto-close
(electric-pair-mode 1)

;; Highlight matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)
;; ------------------------------------

;; ------------------------------------
;; Highlight stuff
;; -----------------
;; Highlight current line
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))

;; Highlight thing at point
(use-package highlight-thing
  :straight t
  :config
  (setq highlight-thing-delay-seconds 0.25))
(global-highlight-thing-mode)

;; Highlight numbers
(use-package highlight-numbers
  :straight t)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; ------------------------------------



;; ------------------------------------------------------------------------
;;; SEARCH & COMPLETION
;; ------------------------------------------------------------------------




;; ------------------------------------------------------------------------
;;; FILE & PROJECT MANAGEMENT
;; ------------------------------------------------------------------------

;; ------------------------------------
;; Disable backups, auto-saves, & lockfiles
;; ----------------------------------------
;; Stop saving #FILE# and FILE~, in the
;; name of all things holy.
(setq make-backup-file nil
      backup-inhibited t
      auto-save-default nil)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Emacs still wants to create the FILE~ backups,
;; so this will put them in their own folder.
(setq backup-directory-alist '(("." . "~/.emacs.trash"))
      backup-by-copying t
      delete-old-versions t)
;; ------------------------------------

;; ------------------------------------
;; Dired
;; -----
;; Stop dired from creating so many buffers
(put 'dired-find-alternate-file 'disabled nil)
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; SYSTEM ADMINISTRATION
;; ------------------------------------------------------------------------




;; ------------------------------------------------------------------------
;;; PROGRAMMING
;; ------------------------------------------------------------------------




;; ------------------------------------------------------------------------
;;; PKMS
;; ------------------------------------------------------------------------




;; ------------------------------------------------------------------------
;;; COMMUNICATIONS
;; ------------------------------------------------------------------------




;; ------------------------------------------------------------------------
;;; LAYOUT AUTOMATION
;; ------------------------------------------------------------------------

;; ------------------------------------
;; Don't save buffers after exit
;; -----------------------------
(desktop-save-mode -1)
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; KEYBINDS & CONTROLS
;; ------------------------------------------------------------------------

;; ------------------------------------
;; Make ESC quit prompts
;; ---------------------
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; ------------------------------------

;; ------------------------------------
;; Replace yes/no prompts with y/n
;; -------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
;; ------------------------------------
