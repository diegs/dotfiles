(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'elpaca)

(use-package emacs
  :ensure nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  :custom
  ;; process performance tuning
  (read-process-output-max (* 4 1024 1024))
  (process-adaptive-read-buffering nil)
  (jit-lock-defer-time 0)
  ;; ui tweaks
  (ring-bell-function #'ignore)
  (blink-cursor-mode nil)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  ;; startup noise
  (inhibit-startup-message t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message (user-login-name))
  ;; backups
  (make-backup-fil nil)
  (auto-save-default nil)
  ;; editor behavior
  (isearch-wrap-pause 'no)
  (sentence-end-double-space nil)
  (require-final-newline t)
  (comment-multi-line t)
  (comment-empty-lines t)
  (kill-do-not-save-duplicates t)
  ;; emacs system
  (custom-buffer-done-kill t)
  (help-window-select t)
  ;; completion
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (prog-mode . (lambda () (setq show-trailing-whitespace t truncate-lines t)))
  (text-mode . (lambda () (setq show-trailing-whitespace t) (visual-line-mode))))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))

(use-package hl-line
  :ensure nil
  :custom
  ;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
  ;; clutter and slightly improving `hl-line-mode' performance.
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  :hook (prog-mode text-mode))

(use-package display-line-numbers
  :ensure nil
  ;; :custom
  ;; (display-line-numbers-type #'relative)
  :hook (prog-mode text-mode))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :demand t
    :config
    (dolist (var '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH" "XDG_DATA_DIRS" "XDG_CONFIG_DIRS" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    :custom
    (exec-path-from-shell-arguments nil)
    :init
    (exec-path-from-shell-initialize)))

;; General performance tuning
(use-package gcmh
  :ensure t
  :demand t
  :diminish
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024))
  :hook elpaca-after-init)

(use-package diminish
  :ensure (:wait t)
  :demand t)

(use-package auto-dark
  :ensure t
  :demand t
  :diminish
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-allow-osascript t)
  (modus-vivendi-palette-overrides '((bg-main "#111111")))
  :init
  (auto-dark-mode t))

(use-package window-divider
  :ensure nil
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  :hook elpaca-after-init)

(use-package meow
  :ensure t
  :demand t
  :config
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode 1)
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-use-clipboard t)
  (meow-select-on-append t)
  (meow-select-on-insert t))

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (tramp-set-completion-function
   "ssh" (append (tramp-get-completion-function "ssh")
                 (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                         (directory-files
                          "~/.ssh/conf.d/"
                          'full directory-files-no-dot-files-regexp))))
  ;(connection-local-set-profile-variables 'remote-direct-async-process '((tramp-direct-async-process . t)))
  ;(connection-local-set-profiles '(:application tramp :protocol "ssh") 'remote-direct-async-process)
  ;(connection-local-set-profiles '(:application tramp :protocol "scp") 'remote-direct-async-process)
  ;(connection-local-set-profiles '(:application tramp :protocol "rsync") 'remote-direct-async-process)
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))
  :custom
  (vc-handled-backends '(Git))
  (remote-file-name-inhibit-locks t)
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 10))
  (tramp-verbose 0)
  (tramp-use-connection-share nil))

(use-package dired
  :ensure nil
  :custom
  (dired-free-space nil))

(use-package ws-butler
  :ensure t
  :diminish
  :hook (prog-mode text-mode))

(use-package eglot
  :ensure nil
  :config
  ;; (add-to-list
  ;;  'eglot-server-programs
  ;;  `((python-ts-mode python-mode) . ("uv" "run" "--with" "python-lsp-server[all]" "pylsp")))
  :custom
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)
  :hook
  (prog-mode . (lambda () (unless (and buffer-file-name (file-remote-p buffer-file-name)) (eglot-ensure)))))

(use-package project
  :ensure nil
  :custom
  ;(projectile-git-submodule-command "true")
  (project-mode-line t))

(use-package corfu
  :ensure t
  ;; Option
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  (corfu-popupinfo-mode))

(when (version< emacs-version "31.0")
  (use-package corfu-terminal
    :ensure t
    :after corfu
    :init
    (unless (display-graphic-p)
      (corfu-terminal-mode +1))))

(use-package vertico
  :ensure t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config
  (keymap-set vertico-map "TAB" #'minibuffer-complete)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package recentf
  :ensure nil
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 1000)
  (recentf-keep '(file-remote-p file-readable-p))
  :init
  (recentf-mode)
  :hook
  (kill-emacs . recentf-cleanup))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :demand t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; embark
;; (setq prefix-help-command #'embark-prefix-help-command)
(use-package nix-mode
  :ensure t)

;(use-package tomlparse :ensure t)
;(use-package yaml :ensure t)

;(use-package pet
;  :ensure t
;  :after (tomlparse yaml)
;  :config
;  :hook python-base-mode)

(provide 'init)
