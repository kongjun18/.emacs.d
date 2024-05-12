(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq-default cursor-type 'bar)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

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
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil)))

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
(add-hook 'after-init-hook 'global-company-mode)
; TODO: preserve <RET>
(add-hook 'after-init-hook 'company-tng-mode)

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

