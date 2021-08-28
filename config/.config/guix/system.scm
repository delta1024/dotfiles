;; NOTE: system.scm is generated from System.org. Please edit that file
;;        in Emacs and system.scm will be generated automatially

(use-modules (gnu)
             (gnu packages shells)
             (gnu services dbus)
             (gnu services pm)
             (nongnu packages linux))
(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg)

(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", ""RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %my-desktop-services
  ;; My personal Desktop configuration
  (modify-services %desktop-services
                   (elogind-service-type config =>
                                         (elogind-configuration
                                          (inherit config)
                                          (handle-lid-switch 'suspend)))
                   ;; (guix-service-type config =>
                   (udev-service-type config =>
                                      (udev-configuration (inherit config)
                                                          (rules (cons %backlight-udev-rule
                                                                       (udev-configuration-rules config)))))
                   ;;(inherit config)
                   ;;do some stuff
                   ));;))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_CA.utf8")
 (timezone "America/Edmonton")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "Cortex")
 (users (cons* (user-account
                (name "jake")
                (comment "Jake")
                (group "users")
                (shell (file-append zsh "/bin/zsh"))
                (home-directory "/home/jake")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "emacs")
         (specification->package "emacs-exwm")
         (specification->package "git")
         (specification->package "stow")
         (specification->package "neovim")
         (specification->package "gcc")
         (specification->package "gnupg")
         (specification->package "xauth")
         (specification->package "zsh")
         (specification->package
          "emacs-desktop-environment")
         (specification->package "nss-certs"))
   %base-packages))
 (services
  (append
   (list (service xfce-desktop-service-type)
         (service cups-service-type)
         (service openssh-service-type)
         (service tlp-service-type
                  (tlp-configuration
                   (cpu-scaling-governor-on-ac (list "performance"))
                   (sched-powersave-on-bat? #t)))
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout)))
         (extra-special-file "/usr/bin/env"
                             (file-append coreutils "/bin/env")))
   %my-desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (timeout 3)
   (keyboard-layout keyboard-layout)))
 (mapped-devices
  (list (mapped-device
         (source
          (uuid "6773b52e-1496-407e-b1d8-9a2ac7f7820f"))
         (target "system-root")
         (type luks-device-mapping))
        (mapped-device
         (source
          (uuid "08123a90-d66b-41ff-8f2c-4435292f7818"))
         (target "crypthome")
         (type luks-device-mapping))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device "/dev/mapper/system-root")
          (type "ext4")
          (dependencies mapped-devices))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "4B6C-4B80" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/home")
          (device "/dev/mapper/crypthome")
          (type "ext4")
          (dependencies mapped-devices))
 
         %base-file-systems))
 (swap-devices
  (list "/tempSwap")))
