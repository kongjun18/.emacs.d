;; -*- lexical-binding: t -*-
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq delete-by-moving-to-trash t)
(if (eq system-type 'darwin)
    (setq trash-directory "~/.Trash"))

;; Use timestamps in English
(setq system-time-locale "C")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;; Solve the problem of Chinese input method get struck
;; See https://emacs-china.org/t/emacs/21861/8
(setq inhibit-compacting-font-caches t)
;;Font size 14pt
(set-face-attribute 'default nil :height 140)
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(defun tune-execpath()
    (set-exec-path-from-shell-PATH)
    (setenv "PATH" (concat (getenv "PATH") ":~/.local/share/nvim/mason/bin"))
    (setq exec-path (append exec-path '("~/.local/share/nvim/mason/bin")))
)
(tune-execpath)
;; GUI improvement based on OS
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(when IS-WINDOWS
  (setq w32-use-native-image-API t))
(unless IS-MAC
  (setq command-line-ns-option-alist nil))
(unless IS-LINUX
  (setq command-line-x-option-alist nil))

;; Chinese configuration
;; See https://github.com/hick/emacs-chinese
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

; (defvar bootstrap-version)
; (let ((bootstrap-file
;        (expand-file-name
;         "straight/repos/straight.el/bootstrap.el"
;         (or (bound-and-true-p straight-base-dir)
;             user-emacs-directory)))
;       (bootstrap-version 7))
;   (unless (file-exists-p bootstrap-file)
;     (with-current-buffer
;         (url-retrieve-synchronously
;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;          'silent 'inhibit-cookies)
;       (goto-char (point-max))
;       (eval-print-last-sexp)))
;   (load bootstrap-file nil 'nomessage))
; (straight-use-package 'use-package)
; (setq straight-use-package-by-default t)

;; ---- Theme ----
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package doom-modeline
    :ensure t
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    :hook
    (after-init . doom-modeline-mode)
    :config
    (line-number-mode 0)
    (column-number-mode 0))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package treesit-auto
  :ensure t
  :config
  (setq my-go-tsauto-config
      (make-treesit-auto-recipe
       :lang 'go
       :revision "v0.20.0"))
(add-to-list 'treesit-auto-recipe-list my-go-tsauto-config)
  (global-treesit-auto-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package keycast
  :ensure t
  :config
  (add-to-list 'global-mode-string '("" mode-line-keycast))
  (keycast-mode-line-mode))


;; ---- Evil ----
;; Enable evil system clipboard
(setq x-select-enable-clipboard nil)

(use-package evil
  :ensure t
  :init
  ;; evil-collection relies on this config
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  (setq evil-want-C-u-scroll t)
  (setq evil-leader/in-all-states 1)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))
  (with-eval-after-load 'evil-maps
    (defun evil-paste-from-clipboard()
      (interactive)
      (evil-paste-from-register ?+))
    (define-key evil-insert-state-map (kbd "C-v") 'evil-paste-from-clipboard)
    (define-key evil-insert-state-map (kbd "C-S-v") 'evil-paste-from-clipboard)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)))

(use-package goto-chg
  :ensure t)
(use-package general
   :ensure t
)
(require 'general)
(general-create-definer my-leader-def
    :states '(normal visual emacs)
    :prefix "SPC")
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))


(my-leader-def "fj" 'evil-collection-consult-jump-list)
(use-package evil-nerd-commenter
  :ensure t
  :config
  (my-leader-def "cc" 'evilnc-comment-or-uncomment-lines)
  (my-leader-def "cy" 'evilnc-copy-and-comment-lines)
  (my-leader-def "cs" 'evilnc-comment-box))

(use-package rainbow-delimiters
    :ensure t
    :hook
    (prog-mode . rainbow-delimiters-mode))

;; --- corfu ---
;; Configure Tempel
(use-package tempel
  :ensure t
  ;; :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
  ;;        ("M-*" . tempel-insert))
)
(use-package tempel-collection
  :ensure t)
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto-prefix 3)          ;; Trigger completion when type 2 char
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match t)        ;; Remove "No Match" message
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-popupinfo-delay '(0.0 . 0.0))
  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :bind (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
	("RET" . corfu-complete))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  ;; Option 1: Specify explicitly to use Orderless for Eglot
(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))
;; Option 2: Undo the Eglot modification of completion-category-defaults
(with-eval-after-load 'eglot
   (setq completion-category-defaults nil))
;; Enable cache busting, depending on if your server returns
;; sufficiently many candidates in the first place.
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
(setq-default eglot-workspace-configuration
      '((haskell (maxCompletions . 200))))
(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'tempel-expand))))
(add-hook 'eglot-managed-mode-hook #'my/eglot-capf))


;; Add extensions
(use-package cape
  :ensure t
  :custom
  (cape-dict-file (file-truename "~/.emacs.d/dict/word.dict"))
  :init
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-file))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)
  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Enable corfu in minibuffer(evil cmdline)
(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)


(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-mode))
(use-package eglot
  :ensure t
  :hook ((prog-mode . eglot-ensure)))
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;; ---- lsp-bridge ----
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown"))

(use-package go-mode
  :ensure t)

;; ---- org-mode ----
(use-package flycheck-aspell
  :ensure t
  :config
  (setq
    ispell-complete-word-dict (file-truename "~/.emacs.d/dict/word.dict")
    ispell-dictionary "en"
    ispell-program-name "aspell"
    ispell-silently-savep t)
    (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
    ;; Because Aspell does not support Org syntax, the user has
    ;; to define a checker with the desired flags themselves.
  (flycheck-aspell-define-checker "org"
    "Org" ("--add-filter" "url")
    (org-mode))
  (add-to-list 'flycheck-checkers 'org-aspell-dynamic))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g n") 'flycheck-next-error)
    (define-key evil-normal-state-map (kbd "g N") 'flycheck-next-error))
;; Adjust margins and fringe widths…
(defun my/set-flycheck-margins ()
  (setq left-fringe-width 30 right-fringe-width 8
        left-margin-width 1 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))

;; …every time Flycheck is activated in a new buffer
(add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)
  )

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
(when (display-graphic-p)
  (cl-loop for font in '("JetBrainsMono Nerd Font" "Cascadia Code" "SF Mono" "Source Code Pro"
                         "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                         "Lucida Console" "Consolas" "SAS Monospace")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                    :weight 'normal
                                    :slant 'normal
                                    :size (cond ((eq system-type 'gnu/linux) 14.0)
                                                ((eq system-type 'windows-nt) 12.5)))))
  (cl-loop for font in '("JetBrainsMono Nerd Font" "OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                         "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode
                                    (font-spec :family font
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0)))
                                    nil 'prepend))
  (cl-loop for font in '("思源宋体 CN" "微软雅黑 CN"
                         "Source Han Sans CN" "Source Han Serif CN"
                         "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                         "Microsoft Yahei UI" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0)))))
  (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p font)
           return (set-fontset-font t '(#x20000 . #x2A6DF)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                           ((eq system-type 'windows-nt) 15.0))))))
(use-package fcitx
  :ensure t
  ;; Only enable fcitx.el on Linux which not runs in SSH
  :if ( and ( = (length (getenv "SSH_TTY")) 0) (eq system-type 'gnu/linux) )
  :init
  (setq fcitx-remote-command "fcitx5-remote")
  :config
  (fcitx-aggressive-setup))

(defun my/org-mode-setup()
  (setq evil-auto-indent nil)
  (my/text-mode-hook-setup)
  (visual-line-mode 1)
  (org-appear-mode))

(use-package org
  :hook (
	 (org-mode . my/org-mode-setup))
  :config
  (setq
	org-startup-indented t
        org-hide-emphasis-markers t
	org-return-follows-link t
        org-mouse-1-follows-link t
	org-use-sub-superscripts '{}
	org-export-with-sub-superscripts nil
        ;; org-agenda
	org-agenda-files (quote ("~/org"))
	org-agenda-diary-file (file-truename "~/org/diary.org")
	org-default-notes-file "~/org/refile.org"
	org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-use-fast-todo-selection t
	org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "WAITING" "INACTIVE" "CANCELED"))
	org-capture-templates
	'(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n")
	  ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %U\n%?")))
  (with-eval-after-load 'org
    (defun my/org-capture-daily ()
   (interactive)
   (org-capture nil "d"))
    (define-key evil-normal-state-map (kbd "C-c C-d") 'my/org-capture-daily)))
(use-package org-appear
  :ensure t
  :config
  (setq org-hide-emphasis-markers t
	org-appear-autolinks t))
(use-package iscroll
  :ensure t
  :diminish iscroll-mode
  :hook ((org-mode markdown-mode) . iscroll-mode))
(use-package org-roam
  :ensure t
  :custom
    (org-roam-directory "~/org")
    (org-roam-completion-everywhere t)
    (org-id-link-to-org-use-id t)
    (org-roam-capture-templates
      '(("d" "default" plain
      "\n%?\n\n----\n* References"
      :if-new (file+head "${title}.org" "#+title: ${title}\n#+created: %U\n#+type: #idea\n#+status: #todo\n")
      :unnarrowed t)))
  :bind (
	:map org-mode-map
	("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))
;; FIXME(kj): Why it doesn't work in :config
(progn
  (my-leader-def "nl" 'org-roam-buffer-toggle)
  (my-leader-def "nf" 'org-roam-node-find)
  (my-leader-def "ni" 'org-roam-node-insert))

;; ;; Make sure org-indent face is available
;; (require 'org-indent)

;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package org-modern
  :ensure t
  :after org
  :config
    ;; Add frame borders and window dividers
    ;; (modify-all-frames-parameters
    ;; '((right-divider-width . 40)
    ;; (internal-border-width . 40)))
    (dolist (face '(window-divider
		    window-divider-first-pixel
		    window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))

    (setq
    ;; Edit settings
    org-auto-align-tags nil
    org-tags-column 0
    org-catch-invisible-edits 'show-and-error
    org-special-ctrl-a/e t
    org-insert-heading-respect-content t

    ;; Org styling, hide markup etc.
    org-hide-emphasis-markers t
    org-pretty-entities t
    org-modern-star 'replace

    ;; Agenda styling
    org-agenda-tags-column 0
    org-agenda-block-separator ?─
    org-agenda-time-grid
    '((daily today require-timed)
    (800 1000 1200 1400 1600 1800 2000)
    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
    org-agenda-current-time-string
    "◀── now ─────────────────────────────────────────────────")

    ;; Ellipsis styling
    (setq org-ellipsis "…")
    (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  (add-hook 'org-mode-hook #'org-modern-mode))
;; ---- magit ----
(use-package magit
  :ensure t)

;; ---- LSP ----
;; Disable eldoc-mode
(global-eldoc-mode -1)
(define-key evil-normal-state-map (kbd "gs") 'xref-find-references)

;; ---- utilities ----
;; Persistent undo
(use-package undohist
  :ensure t
  :init
  (autoload 'undohist-initialize "undohist")
  (undohist-initialize))
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))
(savehist-mode t)
(recentf-mode t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
;; Save auto-save files to ~/.emacs.d/backups
(setq backup-directory-alist
  `(("." . ,(concat user-emacs-directory "backups"))))
;; Show minor mode
(use-package minions
  :ensure t
  :config
  (minions-mode 1))
;; Save command history
(use-package savehist
  :ensure t
  :config
  (savehist-mode 1))
(use-package paren
  :ensure t
  :config
  (show-paren-mode 1))
;; Resotre to previous place
(use-package saveplace
  :config
  (save-place-mode 1))


(use-package orderless
  :ensure t
  :init
  (icomplete-mode)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package pinyinlib
  :ensure t
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (vertico-mode 1)
  :bind ( :map vertico-map
        ("TAB" . vertico-next)
        ([tab] . vertico-next)
        ("S-TAB" . vertico-previous)
        ([backtab] . vertico-previous)))
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind ( :map vertico-map
          ("<backspace>" . vertico-directory-delete-char)))
(use-package vertico-buffer
  :ensure nil
  :config
  (setq vertico-buffer-display-action '(display-buffer-at-bottom))
  (vertico-buffer-mode +1))

(use-package git-gutter
  :ensure t
  :config
    (global-git-gutter-mode t)
    (with-eval-after-load 'evil
	(define-key evil-normal-state-map (kbd "[c") 'git-gutter:previous-hunk)
	(define-key evil-normal-state-map (kbd "]c") 'git-gutter:next-hunk)
	(define-key evil-normal-state-map (kbd "ghr") 'git-gutter:revert-hunk)
	(define-key evil-normal-state-map (kbd "ghs") 'git-gutter:stage-hunk)))
(custom-set-variables
  '(git-gutter:modified-sign "┃")
  '(git-gutter:added-sign "┃")
  '(git-gutter:deleted-sign "_"))

(use-package difftastic
  :ensure t
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))
(use-package rg
  :ensure t
  :defer t)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
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
   consult-theme :preview-key '(:debounce 0 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  (add-to-list 'consult-preview-allowed-hooks 'global-display-line-numbers-mode)
  (add-to-list 'consult-preview-allowed-hooks 'global-treesit-auto-mode)
)
(use-package affe
  :ensure t
  :config
  (my-leader-def "ff" 'affe-find)
  (my-leader-def "fg" 'affe-grep))
(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
(use-package citre
  :ensure t
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-]") 'citre-jump)
  :config
  (setq
   citre-default-create-tags-file-location 'global-cache
   citre-peek-auto-restore-after-jump nil
   citre-peek-fill-fringe nil
   citre-peek-use-dashes-as-horizontal-border t))

;; ---- Obsidian ----
(use-package obsidian
  :ensure t
  :demand t
  :config
  (obsidian-specify-path "~/OneDrive/Obsidian")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "07-笔记")
  ;; Create missing files in inbox? - when clicking on a wiki link
  ;; t: in inbox, nil: next to the file with the link
  ;; default: t
  ;(obsidian-wiki-link-create-file-in-inbox nil)
  ;; The directory for daily notes (file name is YYYY-MM-DD.md)
  (obsidian-daily-notes-directory "06-日记")
  ;; Directory of note templates, unset (nil) by default
  (obsidian-templates-directory "02-模板")
  ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
  ;; (obsidian-daily-note-template "Daily Note Template.md")
  :bind (:map obsidian-mode-map
  ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
  ("C-c C-o" . obsidian-follow-link-at-point)
  ;; Jump to backlinks
  ("C-c C-b" . obsidian-backlink-jump)
  ;; If you prefer you can use `obsidian-insert-link'
  ("C-c C-l" . obsidian-insert-wikilink)))

;; ----- Code -----
(defun delete-this-file (&optional forever)
  "Delete the file associated with `current-buffer'.
If FOREVER is non-nil, the file is deleted without being moved to trash."
  (interactive "P")
  (when-let ((file (or (buffer-file-name)
                       (user-error "Current buffer is not visiting a file")))
             ((y-or-n-p "Delete this file? ")))
    (delete-file file (not forever))
    (kill-buffer (current-buffer))))
