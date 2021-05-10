;; https://gitlab.com/rycee/configurations/blob/master/user/emacs.nix

;; base16.
(require 'base16-theme)
(setq base16-theme-256-color-source "base16-shell")
(load-theme 'base16-horizon-dark t)
;; Color theme.
(defvar my/base16-colors 'base16-horizon-dark)
(setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
      evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))

;; Evil-mode.
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

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

(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2)

(setq-default show-trailing-whitespace t)

(global-display-line-numbers-mode)

(require 'ivy)
(setq ivy-use-virtual-buffers t
      ivy-count-format "%d/%d ")
(ivy-mode 1)
(global-set-key (kbd "C-p") 'counsel-fzf)

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map "\C-p" 'counsel-fzf)
  (define-key evil-normal-state-map "\C-b" 'counsel-switch-buffer))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort))

(require 'projectile)
(projectile-mode +1)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.local/snippets"))
(yas-global-mode 1)

(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(require 'perspective)
(persp-mode)
