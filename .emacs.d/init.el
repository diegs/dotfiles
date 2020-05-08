;; https://gitlab.com/rycee/configurations/blob/master/user/emacs.nix

;; Disable startup message.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Disable some GUI distractions.
;;(tool-bar-mode -1)
;;(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

;; Accept 'y' and 'n' rather than 'yes' and 'no'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't want to move based on visual line.
(setq line-move-visual nil)

;; Stop creating backup and autosave files.
(setq make-backup-files nil
      auto-save-default nil)

;; Always show line and column number in the mode line.
(line-number-mode)
(column-number-mode)

;; Enable some features that are disabled by default.
(put 'narrow-to-region 'disabled nil)

;; Typically, I only want spaces when pressing the TAB key. I also
;; want 4 of them.
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)

;; Trailing white space are banned!
(setq-default show-trailing-whitespace t)

(require 'evil)
(evil-mode 1)

(require 'evil-terminal-cursor-changer)
(evil-terminal-cursor-changer-activate)
