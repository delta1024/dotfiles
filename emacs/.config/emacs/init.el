;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

(defvar emacs-startup-time 
         (format "%.2f seconds"
                 (float-time
                 (time-subtract after-init-time before-init-time))) "Emacs start up time")
(defvar emacs-startup-gc
        gcs-done "Number of garbage collections done at statup")
(defun my/display-startup-time ()
  (message "Emacs loaded in %s."
            emacs-startup-time
           ))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

(setq gc-cons-threshold (* 50 1000 1000))

(defvar my/org-font "Cantarell" "org-mode's variable pitched font name")
(defvar my/user-font "FiraCode NerdFont" "emacs's fixed width font")
(defvar my/font-size 150 "font size for emacs")
(defvar my/emacs-file "Emacs.org" "emacs user file name")
(defvar my/alpha-value '(90 . 90) "EXWM default alpha value")
(defvar my/user-emacs-directory (concat (getenv "HOME") "/.dotfiles/emacs/.config/emacs/")
  "hard coded emacs dir for file comparison")

(setq inhibit-startup-message t)

;; Redirect custom output
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; sets fixed-width font
(set-face-attribute 'default nil :font my/user-font :height my/font-size)


;; Disables the visual bell
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state)
  (evil-mode)
  :bind
  ([remap evil-search-forward] . swiper)
  ([remap evil-search-backward] . swiper-backward))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :after evil
  :config
  (general-evil-setup t))

(general-create-definer my/leader-def
                        :keymaps '(normal insert visual emacs)
                        :prefix "SPC"
                        :global-prefix "C-SPC"
                        :prefix-command 'my-leader-command
                        :prefix-map 'my-leader-map)
(my/leader-def
  "f"     '(nil :which-key "file system")
  "f f"   '(counsel-find-file :which-key "save-file")
  "f s"   '(save-buffer :which-key "save file")
  "h"     '(nil :which-key "config options")
  "h f"   '((lambda () (interactive)
            (find-file (concat user-emacs-directory my/emacs-file))) :which-key "open emacs configuration")
  "h M-f" '((lambda () (interactive)
               (find-file (concat (getenv "HOME") "/.emacs-old/README.org"))) :wk "open old config file")
  "a"     '(eshell :which-key "eshell")
  ";"     '(counsel-M-x :which-key "M-x")
  "w f"   '(delete-frame :wk "delete fram")
  "b"     '(counsel-switch-buffer :wk "switch buffers with preview")
  "M-b"   '(ivy-switch-buffer :wk "switch buffer"))

(use-package org
  :no-require t

:bind ("C-c o" . counsel-outline)

:hook ((org-mode . my/org-mode-setup)
        (org-mode . (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config))))

:config

(defun my/org-font-setup ()
  (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font my/org-font :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))
  (setq org-directory "~/Documents/org")
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)))

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                  (expand-file-name (concat my/user-emacs-directory my/emacs-file)))
;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))

(my/org-font-setup))

(my/leader-def 'org-mode-map
 "'" '(org-edit-special :wk "edit block"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :after org
  :config
  (defun my/org-mode-visual-fill () 
    (setq visual-fill-column-width 115
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  :hook (org-mode . my/org-mode-visual-fill))

(setq org-todo-keywords
  '((sequence "TODO(t)" "STARTEd(s)" "|" "DONE(d!)")
    (sequence "HOLD(h)" "|" "COMPLETED(c)" "DROED(d@)")))

(use-package dired
  :ensure nil
  :after evil
  :demand t)

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer-other-window)))

(use-package ivy-rich
  :after ivy)

(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))
  ;; NOTE: Set this to the folder where you keep your Git repos!

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :config (evil-collection-magit-setup)
  :general
  (:prefix-map 'my-leader-map
   "g" '(magit :which-key "Status")))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :custom ((doom-mode-line-height 13)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun my/exwm-load (switch)
  (load-file (concat user-emacs-directory "desktop.el")))
(add-to-list 'command-switch-alist '("-exwm" . my/exwm-load))

(defun my/post-config () "Sets the `gc-cons-threshold' to a sane value and loads the custom file"
(setq gc-cons-threshold (* 2 1000 1000))
  ;; We're going to load custom here becaus it makes more
  ;; sense to do so here with how EXWM is loaded
  (load custom-file :noerror))

;; Returns nil if switch is abset
(defun found-custom-arg (switch) "Returns nil if switch is absent"
  (let ((found-switch (member switch command-line-args)))
     found-switch))

;; if exwm isn't running set custom variables
(unless (found-custom-arg "-exwm")
  (my/post-config))
