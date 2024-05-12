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

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package keycast
  :ensure t
  :config
  (add-to-list 'global-mode-string '("" mode-line-keycast))
  (keycast-mode-line-mode))

; (use-package evil
;   :ensure t
;   :init
;   (setq evil-want-keybinding nil)
;   (setq evil-want-C-u-scroll t)
;   (evil-mode)
;   :config
;
;   ;; https://emacs.stackexchange.com/questions/46371/how-can-i-get-ret-to-follow-org-mode-links-when-using-evil-mode
;   (with-eval-after-load 'evil-maps
;     (define-key evil-motion-state-map (kbd "RET") nil)))

; --- Company ---
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; --- Company ---
(use-package company
  :init
  (setq company-global-modes '(not erc-mode message-mode eshell-mode))
  (setq company-selection-wrap-around t)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  :ensure t
  :config
  (company-mode)
  (setq company-idle-delay 0)
  (setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
  (add-to-list 'company-backends 'company-c-headers) ;; Backend for header files
  (add-to-list 'company-backends 'company-elisp)
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    ;; remove
    (setq company-backends (delete 'company-clang company-backends))
    ;; add
    ;;(add-to-list 'company-backends 'company-dabbrev)
    )
  :bind (:map company-search-map
              ("C-t" . company-search-toggle-filtering)
              ("TAB" . company-select-next)
              ("<S-tab>" . company-select-previous)
	:map company-active-map
              ("TAB" . company-select-next)
              ("<S-tab>" . company-select-previous)))

