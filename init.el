(global-set-key (kbd "C-c C-c") 'eval-region)
(setenv "HOME" "/home/bachir/.config/emacs")

(setq inhibit-startup-message t)
(setq visible-bell nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Buffer padding
(menu-bar-mode -1)


(set-face-attribute 'default nil :font "Source Code Pro" :height 150)


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(eshell-mode-hook
		term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (use-package command-log-mode)

;; Amongst other things
;; Nice interface for open-file
;; Help with lisp function in the bootom
(use-package ivy
  :diminish
  
  :config
 (ivy-mode 1))

(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x)
	 :map minibuffer-local-map 
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^
  

(use-package swiper)

; Ivy rich
(use-package ivy-rich
  :init (ivy-rich-mode 1)
  )

(define-key emacs-lisp-mode-map (kbd "C-c C-t") 'counsel-load-theme)


;; Theming

; Make sure to run all-the-icons-install-fonts to install all icons
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; (use-package nord-theme
;;   :init
;;   (load-theme 'nord t)
;;   )

  
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  )


(use-package general
  :config
  (general-create-definer bachir/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (bachir/leader-keys
    "SPC" '(counsel-find-file t :which-key "open file")
    "f"   '(:ignore t :which-key "file")
    "ff"  '(counsel-find-file t :which-key "open file")
    "t"   '(:ignore t :which-key "toggles")
    "tt"  '(counsel-load-theme t :which-key "choose theme")))

(general-define-key
 "C-s"   'counsel-grep-or-swiper
 "C-M-j" 'counsel-switch-buffer)


(use-package evil
  :init
  (setq evil-want-integration 1)
  (setq evil-want-keybinding nil)
  (setq evil-cross-lines t)
  ;; :hook
  ;; Disable evil on some modes
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Hydra
(use-package hydra)

(defhydra hydra-zoom ()
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode))



;; Magit
(use-package magit)

  
  ;; :config
  ;; ;; (general-evil-setup t)

  ;; (general-create-definer rune/leader-keys

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hydra evil-collection evil general helpful ivy-rich which-key rainbow-delimiters zones use-package nord-theme doom-themes doom-modeline counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
