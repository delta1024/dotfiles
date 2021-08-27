;; NOTE: early-init.el is now generated from Emacs.org.  Please edit that file
;;         in Emacs and early-init.el will be generated automatically!
(setq inhibit-startup-message t)

;; Redirect custom output

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar
