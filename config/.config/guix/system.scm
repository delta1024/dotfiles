;; NOTE: channels.scm is generated from System.org. Please edit that file
;;        in Emacs and channels.scm will be generated automatially

(list (channel
       (name 'flat)
       (url "https://github.com/flatwhatson/guix-channel.git")
       (commit
        "7e9eb8464f409bc0d9b821ae4caf44c49f23802a")
       (introduction
        (make-channel-introduction
         "33f86a4b48205c0dc19d7c036c85393f0766f806"
         (openpgp-fingerprint
          "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (commit
        "4028f64b583ea4a55d79d270e810146d63d43983")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit
        "a1dbc8be95369f4ff805117b8966dfd1364c396f")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

;; NOTE: system.scm is generated from System.org. Please edit that file
;;        in Emacs and system.scm will be generated automatially

(use-modules (gnu)
             (gnu packages shells)
             (nongnu packages linux))
(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg)

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
         ;; (service elogind-service-type
         ;;          (elogind-configuration
         ;;           (handle-lid-switch 'suspend)))
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout)))
         (extra-special-file "/usr/bin/env"
                             (file-append coreutils "/bin/env")))
   %desktop-services))
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
