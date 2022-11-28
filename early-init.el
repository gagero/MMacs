;;; early-init.el --- Early init file for MMacs -*- lexical-binding t; -*-
;; Author: MountainMan

;; ------------------------------------------------------------------------
;;; COMMENTARY
;; ------------------------------------------------------------------------

;; ------------------------------------
;; ABOUT EARLY INIT
;; ----------------
;; Early init allows configuration code
;; to run early in the startup process,
;; before the GUI loads.
;; ------------------------------------

;; ------------------------------------
;; FILE CONTENTS
;; -------------
;; COMMENTARY
;; - ABOUT EARLY INIT
;; STARTUP
;; - GARBAGE COLLECTION
;; - EMACS SERVER
;; - UI ADJUSTMENTS
;; - IGNORE XRESOURCES
;; - FIX FOR STRAIGHT.EL
;; - END OF EARLY-INIT
;; ------------------------------------




;; ------------------------------------------------------------------------
;;; STARTUP
;; ------------------------------------------------------------------------

;; ------------------------------------
;; Garbage collection
;; ------------------------------------
;; Defer garbage collection.
;; Garbage collection is handled by
;; gcmh later in the startup process.

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; ------------------------------------

;; ------------------------------------
;; Emacs daemon/server
;; -------------------
(server-start)
;; ------------------------------------

;; ------------------------------------
;; UI Adjustments
;; --------------
;; Removing these UI elements before the
;; GUI loads prevents a bit of stuttering
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; On startup, Emacs flashes white until
;; the theme is applied. Set BG/FG color
;; here to fix this issue.
(set-face-attribute 'default nil :background "#333333" :foreground "#f5f5f5" :height 120)
;; ------------------------------------

;; ------------------------------------
;; Inhibit implied resize
;; ----------------------
;; Resizing the Emacs frame can be a terribly
;; expensive part of changing the font. By
;; inhibiting this, we easily halve startup
;; times with fonts that are larger than the
;; system default.

(setq frame-inhibit-implied-resize t)
;; ------------------------------------

;; ------------------------------------
;; Ignore X resources
;; ------------------
;; It's settings would be redundant with the other
;; settings in this file and can conflict with later
;; config (particularly where the cursor color is concerned).

(advice-add #'x-apply-session-resources :override #'ignore)
;; ------------------------------------

;; ------------------------------------
;; Straight.el fix
;; ---------------
(setq package-enable-at-startup nil)
;; ------------------------------------

;; ------------------------------------------------------------------------
;;; END OF EARLY-INIT
;; ------------------------------------------------------------------------
