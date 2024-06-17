(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq-default cursor-type 'bar)
(setq delete-by-moving-to-trash t)
;; Use timestamps in English
(setq system-time-locale "C")
;; Font size 14pt
(set-face-attribute 'default nil :height 180)
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
  :demand t
  :config
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
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-leader/in-all-states 1)
  (setq evil-want-minibuffer t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    ))

(use-package general
   :ensure t
)
(require 'general)
(general-create-definer my-leader-def
    :states '(normal visual emacs)
    :prefix "SPC")

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (my-leader-def "cc" 'evilnc-comment-or-uncomment-lines)
  (my-leader-def "cy" 'evilnc-copy-and-comment-lines)
  (my-leader-def "cs" 'evilnc-comment-box))

;; --- Company ---
(defun customed-compnay-mode ()
  ; company-tng-mode must be load before company-quickhelp-mode,
  ; otherwise, company-tng-mode doesn't work.
  (company-tng-mode)
  (company-quickhelp-mode))

(use-package company
 :ensure t
 :config
 (setq company-idle-delay 0)
 (setq company-selection-wrap-around t)
 (customed-compnay-mode)
  :bind (:map company-search-map
              ("C-t" . company-search-toggle-filtering)
              ("TAB" . company-select-next)
              ("<backtab>" . company-select-previous)
              ("RET" . company-complete)
    :map company-active-map
              ("TAB" . company-select-next)
              ("<backtab>" . company-select-previous))
              ("RET" . company-complete))

(add-hook 'after-init-hook 'global-company-mode)
;; Don't enable company-mode in below major modes, OPTIONAL
(setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))

(defun toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(use-package company-quickhelp
  :ensure t
  :init
  (setq company-quickhelp-delay 0.001)
  :after company)


(use-package consult-eglot
    :ensure t)
(use-package eglot
  :after company
  :ensure t
  :config
  (add-hook 'go-mode-hook 'eglot-ensure))
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

(use-package company-dict
  :ensure t
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  (add-to-list 'company-backends 'company-dict)
  :bind (:map evil-insert-state-map
              ("C-x C-k" . company-dict))
)

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

; (use-package lsp-bridge
;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;             :build (:not compile))
;   :init
;   (setq acm-enable-preview t)
;   (global-lsp-bridge-mode)
;   :config
;   (define-key acm-mode-map [tab] 'acm-select-next)
;   (define-key acm-mode-map [backtab] 'acm-select-prev))

;; ---- org-mode ----
(use-package fcitx
  :ensure t
  ;; Only enable fcitx.el on Linux which not runs in SSH
  :if ( and ( = (length (getenv "SSH_TTY")) 0) (eq system-type 'gnu/linux) )
  :init
  (setq fcitx-remote-command "fcitx5-remote")
  :config
  (fcitx-aggressive-setup))

(defun my/text-mode-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell)
  (setq
   company-ispell-dictionary (file-truename "~/.emacs.d/dict/word.dict")
   ispell-complete-word-dict (file-truename "~/.emacs.d/dict/word.dict")))
(add-hook 'text-mode-hook 'my/text-mode-hook-setup)
(defun my/org-mode-setup()
  (my/text-mode-hook-setup)
  (org-indent-mode)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (
	 (org-mode . my/org-mode-setup))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        ;; org-agenda
	org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	))

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

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; ---- magit ----
(use-package magit
  :ensure t)

;; ---- LSP ----
;; Disable eldoc-mode
(global-eldoc-mode -1)
(define-key evil-normal-state-map (kbd "gs") 'xref-find-references)

;; ---- utilities ----
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))
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
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
)

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
