;; https://gitlab.com/rycee/configurations/blob/master/user/emacs.nix

;; Evil-mode first.
(setq evil-want-C-u-scroll t)
(setq evil-undo-system 'undo-tree)
(require 'evil)
(evil-mode 1)

;; SYSTEM CUSTOMIZATIONS.

;; Disable backups and autosaves.
(setq make-backup-files nil
      auto-save-default nil)

;; Accept 'y' and 'n' rather than 'yes' and 'no'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Misc tweaks stolen from better-defaults.
(setq save-interprogram-paste-before-kill t
    apropos-do-all t
    mouse-yank-at-point t
    require-final-newline t
    visible-bell t
    load-prefer-newer t
    ediff-window-setup-function 'ediff-setup-windows-plain
    custom-file (expand-file-name "~/.emacs.d/custom.el"))

;; TODO: figure out if I want these in evil-mode.
;(global-set-key (kbd "M-/") 'hippie-expand)
;(global-set-key (kbd "C-x C-b") 'ibuffer)
;(global-set-key (kbd "M-z") 'zap-up-to-char)
;
;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;(global-set-key (kbd "C-M-s") 'isearch-forward)
;(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Enable useful default plugins/features.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(save-place-mode 1)
(savehist-mode 1)
(put 'narrow-to-region 'disabled nil)

;; UI CUSTOMIZATIONS.

;; Disable startup message.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Hide GUI stuff.
(unless (eq window-system 'ns) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Base16.
(require 'base16-theme)
(setq base16-theme-256-color-source "base16-shell")
(load-theme 'base16-harmonic-dark t)
;; Color theme.
(defvar my/base16-colors 'base16-harmonic-dark)
(setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
      evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))

;; Frame settings.
(line-number-mode)
(setq display-line-numbers-type 'relative)
(column-number-mode)
(setq-default show-trailing-whitespace t)
(show-paren-mode 1)
(global-display-line-numbers-mode)

;; EDITING CUSTOMIZATIONS.

;; Don't move based on visual line.
(setq line-move-visual nil)

;; Sane indentation settings.
(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2)

;; PLUGINS

;; selectrum.
(require 'selectrum)
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
(marginalia-mode)

;; company.
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort))

;; yasnippet.
(require 'yasnippet)
(yas-global-mode +1)

;; tree-sitter.
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; eglot.
(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

;(defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
;(add-hook 'eglot--managed-mode-hook
;          (lambda () (add-hook 'before-save-hook '+eglot-organize-imports nil 'local)))

;; Dummy go-mode since tree-sitter and LSP do all the work.
; (define-generic-mode 'go-mode nil nil nil '("\\.go$") nil "A mode for Go files")

;; Evil keymaps.
(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map "\C-p" 'project-find-file)
  (define-key evil-normal-state-map "\C-b" 'consult-buffer))
