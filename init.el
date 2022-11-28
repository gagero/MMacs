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
;; PROJECT MANAGEMENT
;; PROGRAMMING LANGUAGES
;; SYSTEM ADMINISTRATION
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
