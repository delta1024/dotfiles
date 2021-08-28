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
(defvar my/user-font "Fira Code" "emacs's fixed width font")
(defvar my/font-size 150 "font size for emacs")
(defvar my/emacs-file (expand-file-name  ".dotfiles/Emacs.org" (getenv "HOME")) "emacs configuration file name")
(defvar my/guix-file (expand-file-name  ".dotfiles/System.org" (getenv "HOME")) "GNU Guix configuration file")
(defvar my/alpha-value '(90 . 90) "EXWM default alpha value")

(setq inhibit-startup-message t)

;; Redirect custom output

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; sets fixed-width font
(set-face-attribute 'default nil :font my/user-font :height my/font-size)


;; Disables the visual bell
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                markdown-mode
                eshell-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;(require 'package)

;;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;("elpa" . "https://elpa.gnu.org/packages/")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)

(use-package evil
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
  :prefix "C-SPC"
  :global-prefix "C-SPC"
  :prefix-command 'my-leader-command
  :prefix-map 'my-leader-map)

(my/leader-def
  "f"     '(nil                                                     :wk "file system")
  "f f"   '(find-file                                               :wk "save-file")
  "f s"   '(save-buffer                                             :wk "save file")
  "f r"   '((lambda () (interactive) (find-file "/sudo::"))         :wk "open file as root")
  "h"     '(nil                                                     :wk "config options")
  "h f"   '((lambda () (interactive)
              (find-file my/emacs-file))                            :wk "open emacs configuration")
  "h M-f" '((lambda () (interactive)
              (find-file my/guix-file))                             :wk "open guix file")
  "a"     '(eshell                                                  :wk "eshell")
  ";"     '(execute-extended-command                                :wk "M-x")
  "w f"   '(delete-frame                                            :wk "delete fram")
  "b"     '(consult-buffer                                          :wk "switch buffers with preview")
  ;;"M-b"   '(ivy-switch-buffer                                       :wk "switch buffer")
  "C-s"   '((lambda () (interactive) (guix))                        :wk "Guix")
  "o"     '(nil                                                     :wk "org")
  "o f"   '(my/org-open-file                                        :wk "open org file")
  "o a"   '(org-agenda                                              :wk "org agenda")
  "c"     '(org-capture                                             :wk "change directory"))

(use-package swiper)

(customize-set-variable 'org-directory "~/Documents/org/")
(customize-set-variable 'org-archive-location "~/Documents/org/archive.org")
(setq org-default-notes-file (expand-file-name "Tasks.org" org-directory))
(setq org-agenda-files '("Tasks.org"))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        (".archive.org" :maxlevel . 1)))

;; Save Org buffers after refilling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
        (sequence "HOLD(h)" "|" "COMPLETED(c)" "DROED(D@)")
        (sequence "NOT_BOOKED" "|" "BOOKED")))

(setq org-capture-templates
      '(("t" "TODO")
        ("tg" "General" entry (file+olp "~/Documents/org/Tasks.org" "General")
         "* TODO %^{Title}\n %?")
        ("th" "House" entry (file+olp "~/Documents/org/Tasks.org" "Household")
         "* TODO %?\n")
        ("tm" "Medical" entry (file+olp "~/Documents/org/Tasks.org" "Medical")
         "* %^{Status|MEDICAL|NOT_BOOKED|BOOKED} %?\nDoctor: %^{Doctor|Mc'G|Lewis|Shell}\nDate: ")

        ("c" "Configs")
        ("ce" "Emacs")
        ("ceo" "Org" entry (file+olp "~/.dotfiles/Emacs.org" "Inbox" "Org")
         "* TODO %^{Title}\nDescription: %?")
        ("cee" "Emacs" entry (file+olp "~/.dotfiles/Emacs.org" "Inbox" "General")
         "* %^{Title}\n%?")

        ("cd" "Desktop")
        ("cdk" "Keybindings" entry (file+olp "~/.dotfiles/Desktop.org" "Inbox" "Keybindings")
         "* TODO %^{Function: }\nBinding: =%^{Binding}=\nMap: %^{Keymap: }")
        ("cdw" "Windows" entry (file+olp "~/.dotfiles/Desktop.org" "Inbox" "Windows")
         "* TODO %^{Window}\nDesired Behaviour:%?")
        ("cdg" "General" entry (file+olp "~/.dotfiles/Desktop.org" "Inbox" "General")
         "* TODO %?")

        ("cs" "System")
        ("cso" "Os" entry (file+olp "~/.dotfiles/System.org" "Inbox" "Os")
         "* TODO %^{Title}\n%?")
        ("csm" "Manifests" entry (file+olp "~/.dotfiles/System.org" "Inbox" "Manifests" "Inbox")
         "* %^{Package name: }\nManifest: %^{Manifest: }")
        ("csg" "General" entry (file+olp "~/.dotfiles/System.org" "Inbox" "General")
         "* TODO %?")))

(defun my/org-open-file (a)  "Opens the file in `org-directory'"
       (interactive "sOrg File: ")
       (find-file (expand-file-name (concat a ".org") org-directory)))

(use-package org
  :straight t
  :no-require t
  :bind ((:map org-mode-map
               ("C-c o" . consult-outline)))
  ([remap evil-jump-forward] . org-cycle)
  :hook (org-mode . my/org-mode-setup)
  (org-mode . (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))
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
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
  (defun my/org-babel-tangle-config ()
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name "~/.dotfiles/"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))
  (my/org-font-setup))

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
  :hook (org-mode . my/org-mode-visual-fill)
  (markdown-mode . my/org-mode-visual-fill))

(use-package emacsql
  :straight t)
(use-package emacsql-sqlite
  :straight t)
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dired
  :after evil
  :demand t
  :commands (dired dired-jump)
  :hook (dired-mode . dired-hide-details-mode)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  (setq dired-always-read-filesystem t
        dired-switches-in-mode-line t)
  :custom ((dired-listing-switches "-aBGgD --group-directories-first")))

(use-package dired-single
  :straight t)

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "l" 'dired-single-buffer)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package dired-hide-dotfiles
;;   :straight t
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "H" 'dired-hide-dotfiles-mode))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  (setq vertico-resize t)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        selectrum-highlight-candidates-function #'orderless-highlight-matches))

;; Persist history over Emacs restarts. Vertico sorts by history position. 
(use-package savehist
  :init
  (savehist-mode))

(use-package selectrum)

(use-package consult
  :bind
  ("C-s" . consult-line))

(use-package embark

  :bind
  (("M-o" . embark-act))         ;; pick some comfortable binding

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :config
  (dolist (profiles '("browsers/browsers/share/applications"
                      "apps/apps/share/applications"
                      "desktop/desktop/share/applications"
                      "emacs/emacs/share/applications"))
    (add-to-list 'app-launcher-apps-directories (concat (getenv "GUIX_EXTRA_PROFILES") "/" profiles))))

(use-package ivy
  :disabled t
  :diminish t
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)	
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)
              ("C-j" . ivy-next-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-j" . ivy-next-line)
              ("C-d" . ivy-reverse-i-search-kill)))

(use-package counsel
  :disabled t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer-other-window))
  :custom
  ((counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)))

(use-package ivy-rich
  :disabled
  :after ivy)

(use-package projectile
  :diminish projectile-mode
  ;;:custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))
;; NOTE: Set this to the folder where you keep your Git repos!

(use-package counsel-projectile
  :disabled t
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :config (evil-collection-magit-setup)
  :general
  (:prefix-map 'my-leader-map
               "g" '(magit :which-key "Status")))

(use-package pass)
(use-package pinentry
  :config
  (pinentry-start))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom ((doom-mode-line-height 13)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun my/exwm-load (switch)
  (load-file (expand-file-name "desktop.el" user-emacs-directory )))
;;  (load-file (expand-file-name "desktop.el" user-emacs-directory))
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
