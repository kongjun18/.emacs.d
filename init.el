
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq-default cursor-type 'bar)

;; Font size 14pt
(set-face-attribute 'default nil :height 140)

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

;; ---- Modus Theme ----
;; Configure the Modus Themes' appearance
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1))))

;; Load the dark theme by default
(load-theme 'modus-vivendi t)


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
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil)))

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

(use-package company
  :init
  ;QUESTION: Why not work?
  ;;(setq company-global-modes '(not erc-mode message-mode eshell-mode))
  (setq company-selection-wrap-around t)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'emacs-go-mode-hook 'company-mode)
  :ensure t
  :config
  (company-mode)
  (setq company-idle-delay 0)
  ;(setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
  (add-to-list 'company-backends 'company-elisp)
  (add-to-list 'company-backends 'company-go)
  (add-to-list 'company-backends 'company-dict)
  :bind (:map company-search-map
              ("C-t" . company-search-toggle-filtering)
              ("TAB" . company-select-next)
              ("<backtab>" . company-select-previous)
              ("RET" . company-complete)
    :map company-active-map
              ("TAB" . company-select-next)
              ("<backtab>" . company-select-previous))
              ("RET" . company-complete))
; TODO: preserve <RET>

(use-package company-quickhelp
  :ensure t
  :init
  (setq company-quickhelp-delay 0.001)
  :after company)

(defun customed-compnay-mode ()
  "add all company-related modes"
  (global-company-mode)
  ; company-tng-mode must be load before company-quickhelp-mode,
  ; otherwise, company-tng-mode doesn't work.
  (company-tng-mode)
  (company-quickhelp-mode))
(add-hook 'after-init-hook 'customed-compnay-mode)

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

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (org-num-mode)
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

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


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

;; ---- utilities ----
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
