(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq-default cursor-type 'bar)

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

;; Configure the Modus Themes' appearance
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      ;; FIXME(kj): Warrning
      ;; modus-themes-completions 'opinionated
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

(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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
  ; (when (boundp 'company-backends)
    ; (make-local-variable 'company-backends)
    ; ;; remove
    ; (setq company-backends (delete 'company-clang company-backends))
    ; ;; add
    ; ;;(add-to-list 'company-backends 'company-dabbrev)
    ; )
  :bind (:map company-search-map
              ("C-t" . company-search-toggle-filtering)
              ("TAB" . company-select-next)
              ("<backtab>" . company-select-previous)
	:map company-active-map
              ("TAB" . company-select-next)
              ("<backtab>" . company-select-previous)))
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

(use-package fcitx
  :ensure t
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
