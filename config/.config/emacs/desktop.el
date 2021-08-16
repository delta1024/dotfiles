;; NOTE: desktop.el is now generated from Desktop.org.  Please edit that file
;;       in Emacs and desktop.el will be generated automatically!

(defvar my/exwm-config "Desktop.org" "EXWM Configuration file name")
(add-to-list 'default-frame-alist '(alpha 90 90))
(my/leader-def
  "h C-f" '((lambda () (interactive)
               (find-file (concat my/user-emacs-directory my/exwm-config))) :wk "open desktop configuration"))

(with-eval-after-load 'org
  (defun my/org-babel-tangle-desktop ()
    (when (string-equal (buffer-file-name)
                    (expand-file-name my/exwm-config))
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook
                                        #'my/org-babel-tangle-desktop))))

(defvar my/exwm-auto-start-apps '(("xmodmap" . "xmodmap ~/.Xmodmap")))

(defun my/exwm-auto-start ()
 (interactive)
 (dolist (process my/exwm-auto-start-apps)
   (start-process-shell-command (car process) nil (cdr process))))

(defun my/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'my/exwm-update-class)

;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
       ?\C-u
       ?\C-h
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

(exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

(defun exwm-poweroff ()
  (interactive)
  (when (yes-or-no-p "Power down system? ")
    (start-process-shell-command "systemctl" nil "systemctl poweroff")))
(exwm-input-set-key (kbd "C-s-p") 'exwm-poweroff)

(defun exwm-suspend ()
  (interactive)
  (when (y-or-n-p "Suspend systeM? ")
    (start-process-shell-command "systemctl" nil "systemctl suspend")))
(exwm-input-set-key (kbd "s-p") 'exwm-suspend)

(defun exem-sys-restart () "Restarts the operating system"
       (interactive)
       (when (yes-or-no-p "Restart system? ")
         (start-process-shell-command "systemctl" nil "systemctl restart")))
(exwm-input-set-key (kbd "M-s-p") 'exwm-sys-restart)
(exwm-input-set-key (kbd "s-q") 'exwm-restart)

(exwm-enable)
(my/exwm-auto-start))

;; Show battery status in the mode line
(display-battery-mode 1)

;; Show the time and date in modeline
(setq display-time-day-and-date t)
(display-time-mode 1)
;; Also take a look at display-time-format and format-time-string

(my/post-config)
