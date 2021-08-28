;; NOTE: desktop.el is now generated from Desktop.org.  Please edit that file
;;       in Emacs and desktop.el will be generated automatically!

(defvar exwm-gcs-start-var gcs-done "numer of gc's done at the begining of destkop.el")
(server-start)
(defvar my/exwm-config (expand-file-name ".dotfiles/Desktop.org" (getenv "HOME")) "EXWM Configuration file name")
(add-to-list 'default-frame-alist '(alpha 90 90))
(my/leader-def
  "h C-f" '((lambda () (interactive)
               (find-file my/exwm-config)) :wk "open desktop configuration"))

(defvar my/picom-pid nil
  "picom process id, if any")
(defun my/kill-picom () "kills the runnign picom procrss"
       (interactive)
       (when my/picom-pid
         (ignore-errors
           (kill-process my/picom-pid)))
       (setq my/picom-pid nil))
(defun my/start-picom () "starts picom pointing to the default configuation location"
       (interactive)
       (my/kill-picom)
       (setq my/picom-pid (start-process-shell-command "picom" nil "picom")))
(defun my/toggle-picom () "Toggles picom"
(interactive)
(if my/picom-pid
(my/kill-picom)
(my/start-picom)))
;; (defun efs/polybar-exwm-workspace ()
;;   (pcase exwm-workspace-current-index
;;     (0 "")
;;     (1 "")
;;     (2 "")
;;     (3 "")
;;     (4 "")))
(defun my/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun my/send-polybar-exwm-workspace ()
  (my/send-polybar-hook "exwm-workspace" 1))

(defvar my/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun my/kill-panel ()
  (interactive)
  (when my/polybar-process
    (ignore-errors
      (kill-process my/polybar-process)))
  (setq my/polybar-process nil))

(defun my/start-panel ()
  (interactive)
  (my/kill-panel)
  (setq my/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun my/toggle-panel ()
  (interactive)
  (if my/polybar-process
      (my/kill-panel)
    (my/start-panel)))


(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun my/exwm-init-hook ()
  (eshell)
  (my/start-panel)
  (my/start-picom))

;; (my/run-in-background "picom")
;; (my/run-in-background "xclip")
;; (my/run-in-background (concat (getenv "HOME") "/" ".scripts/wallpaper.sh draw")))

(defun my/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package desktop-environment
  :defer t)
(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)
  (add-hook 'exwm-init-hook #'my/exwm-init-hook)
  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'my/exwm-update-class)
  ;; Update panel indicator when workspace changes
  (add-hook 'exwm-workspace-switch-hook #'my/send-polybar-exwm-workspace)
  (add-hook 'exwm-init-hook #'my/post-config)

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
  
  (exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app) ;; Set XDG_PATH variables
  (exwm-input-set-key (kbd "s-y") '(lambda () (interactive)
         (start-process-shell-command "Wallpaper" nil "~/.scripts/wallpaper.sh set")))
  (exwm-input-set-key (kbd "s-p") 'my/toggle-panel)
  (exwm-input-set-key (kbd "s-g") 'pass)
  (exwm-input-set-key (kbd "C-s-p") 'my/toggle-picom)
  (exwm-input-set-key (kbd "s-s") '(lambda (query)
                                     (interactive "sWeb Search: ")
                                     (start-process-shell-command "qutebrowser" nil (concat "qutebrowser ""\"" query "\""))))
  (exwm-input-set-key (kbd "s-f") '(lambda ()
                                     (interactive)
                                     (start-process-shell-command "firefox" nil "firefox youtube.com")))
  (exwm-enable))

(defvar exwm-gc-end-var gcs-done "number of gc's done at end of desktop.el in total")
(defvar my/desktop-gs (- exwm-gc-end-var exwm-gcs-start-var) "number of gc's done durring desktop.el evaluation")
