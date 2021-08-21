;; NOTE: desktop.el is now generated from Desktop.org.  Please edit that file
;;       in Emacs and desktop.el will be generated automatically!

(server-start)
(defvar my/exwm-config (concat (getenv "HOME") "/.dotfiles/Desktop.org") "EXWM Configuration file name")
(add-to-list 'default-frame-alist '(alpha 90 90))
(my/leader-def
  "h C-f" '((lambda () (interactive)
               (find-file my/exwm-config)) :wk "open desktop configuration"))

(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun my/exwm-init-hook ()
  (eshell)
  (my/run-in-background "picom")
  (my/run-in-background "xclip")
  (my/run-in-background (concat (getenv "HOME") "/" ".scripts/wallpaper.sh draw")))

(defun my/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package desktop-environment
  :defer t)
(use-package exwm
  :ensure nil
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)
  (add-hook 'exwm-init-hook #'my/exwm-init-hook)
  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'my/exwm-update-class)

  (desktop-environment-mode)
  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\C-w
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\ ))  ;; Ctrl+Space
  
  ;;    Ctrl+ Q will enable the next key to
  ;;    be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  
  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)
  
          ;; Move between windows
          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)
  
          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
  
          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
  
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app) ;; Set XDG_PATH variables
  
  (defun exwm-change-wallpaper () "Changes the Wallpaper"
    (interactive)
    (start-process-shell-command "Wallpaper" nil "~/.scripts/wallpaper.sh set"))
  (exwm-input-set-key (kbd "s-y") 'exwm-change-wallpaper)
  
  (exwm-enable))
;; Show battery status in the mode line
(display-battery-mode 1)

;; Show the time and date in modeline
(setq display-time-day-and-date t)
(display-time-mode 1)
;; Also take a look at display-time-format and format-time-string


