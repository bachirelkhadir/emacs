(global-set-key (kbd "C-c C-c") 'eval-region)
(setenv "HOME" "/home/bachir/.config/emacs")
(setq  org-confirm-babel-evaluate nil)

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

(defun bachir/org-babel-tangle-config()
  (when (string-equal (buffer-file-name) 
		      (expand-file-name "/home/bachir/.config/emacs/init.org"))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'bachir/org-babel-tangle-config)))

;; (setq inhibit-startup-message t)
(setq visible-bell nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Buffer padding
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

; Font
(set-face-attribute 'default nil :font "Source Code Pro" :height 150)
(set-fontset-font t 'latin "Noto Sans")

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

(defun bachir/org-mode-setup()
  (org-indent-mode))

(use-package org
  :config
  (setq org-ellipsis " ▼")
  (setq org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (use-package org-make-toc
;;   :after org)

(use-package org-tree-slide)

(use-package hydra
  :bind (("C-M-S-i" . hydra-image/body)
         ("C-M-S-l" . hydra-ledger/body)
         ("C-M-S-M" . hydra-merge/body)
         ("C-M-S-t" . hydra-tool/body)
         ("C-M-S-b" . hydra-btoggle/body)
         ("C-M-S-c" . hydra-clock/body)
         ("C-M-S-e" . hydra-erc/body)
         ("C-M-S-f" . hydra-flycheck/body)
         ("C-M-S-g" . hydra-go-to-file/body)
         ("C-M-S-m" . hydra-magit/body)
         ("C-M-S-o" . hydra-org/body)
         ("C-M-S-p" . hydra-projectile/body)
         ("C-M-S-q" . hydra-query/body)
         ("C-M-S-s" . hydra-spelling/body)
         ("C-M-S-t" . hydra-tex/body)
         ("C-M-S-u" . hydra-upload/body)
         ("C-M-S-w" . hydra-windows/body)))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t))
   "Coding"
   (("e" electric-operator-mode "electric operator" :toggle t)
    ("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t))
   "UI"
   (("i" ivy-rich-mode "ivy-rich" :toggle t))))

(pretty-hydra-define hydra-clock
  (:hint nil :color teal :quit-key "q" :title (with-faicon "clock-o" "Clock" 1 -0.05))
  ("Action"
   (("c" org-clock-cancel "cancel")
    ("d" org-clock-display "display")
    ("e" org-clock-modify-effort-estimate "effort")
    ("i" org-clock-in "in")
    ("j" org-clock-goto "jump")
    ("o" org-clock-out "out")
    ("p" org-pomodoro "pomodoro")
    ("r" org-clock-report "report"))))

(pretty-hydra-define hydra-erc
  (:hint nil :color teal :quit-key "q" :title (with-faicon "comments-o" "ERC" 1 -0.05))
  ("Action"
   (("b" my/erc-browse-last-url "browse last url")
    ("c" my/erc-start-or-switch "connect")
    ("d" erc-quit-server "disconnect")
    ("j" erc-join-channel "join")
    ("n" erc-channel-names "names")
    ("o" my/erc-get-ops "ops")
    ("u" my/erc-count-users "users")
    ("r" my/erc-reset-track-mode "reset track mode"))))

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(pretty-hydra-define hydra-go-to-file
  (:hint nil :color teal :quit-key "q" :title (with-faicon "file-text-o" "Go To" 1 -0.05))
  ("Agenda"
   (("ac" (find-file "~/.personal/agenda/contacts.org") "contacts")
    ("ao" (find-file "~/.personal/agenda/organizer.org") "organizer")
    ("ap" (find-file "~/.personal/agenda/people.org") "people")
    ("ar" (find-file "~/.personal/agenda/routine.org") "routine")
    ("as" (find-file "~/.personal/agenda/school.org") "school"))
   "Config"
   (("ca" (find-file (format "%s/alacritty/alacritty.yml" xdg-config)) "alacritty")
    ("cA" (find-file (format "%s/sh/aliases" xdg-config)) "aliases")
    ("cd" (find-file (format "%s/dunst/dunstrc" xdg-config)))
    ("ce" (find-file "~/.emacs.d/config.org") "emacs")
    ("cE" (find-file (format "%s/sh/environ" xdg-config)) "environ")
    ("cn" (find-file (format "%s/neofetch/config.conf" xdg-config)) "neofetch")
    ("cq" (find-file (format "%s/qutebrowser/config.py" xdg-config)) "qutebrowser")
    ("cr" (find-file (format "%s/ranger/rc.conf" xdg-config)) "ranger")
    ("cs" (find-file (format "%s/sway/config" xdg-config)) "sway")
    ("ct" (find-file (format "%s/tmux/tmux.conf" xdg-config)) "tmux")
    ("cw" (find-file (format "%s/waybar/config" xdg-config)) "waybar")
    ("cW" (find-file (format "%s/wofi/config" xdg-config)) "wofi")
    ("cx" (find-file (format "%s/sh/xdg" xdg-config)) "xdg"))
   "Other"
   (("ob" (find-file "~/.personal/other/books.org") "book")
    ("ol" (find-file "~/.personal/other/learning.org") "learning")
    ("om" (find-file "~/.personal/other/movies.org"))
    ("op" (find-file "~/.personal/other/purchases.org") "purchase")
    ("ou" (find-file "~/.personal/other/usb.org") "usb"))))

(pretty-hydra-define hydra-image
  (:hint nil :color pink :quit-key "q" :title (with-faicon "file-image-o" "Images" 1 -0.05))
  ("Action"
   (("r" image-rotate "rotate")
    ("s" image-save "save" :color teal))
    "Zoom"
    (("-" image-decrease-size "out")
     ("+" image-increase-size "in")
     ("=" image-transform-reset "reset"))))

(pretty-hydra-define hydra-ledger
  (:hint nil :color teal :quit-key "q" :title (with-faicon "usd" "Ledger" 1 -0.05))
  ("Action"
   (("b" leadger-add-transaction "add")
    ("c" ledger-mode-clean-buffer "clear")
    ("i" ledger-copy-transaction-at-point "copy")
    ("s" ledger-delete-current-transaction "delete")
    ("r" ledger-report "report"))))

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
  ("Move"
   (("n" smerge-next "next")
    ("p" smerge-prev "previous"))
   "Keep"
   (("RET" smerge-keep-current "current")
    ("a" smerge-keep-all "all")
    ("b" smerge-keep-base "base")
    ("l" smerge-keep-lower "lower")
    ("u" smerge-keep-upper "upper"))
   "Diff"
   (("<" smerge-diff-base-upper "upper/base")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "redefine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill current"))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
    ("s" counsel-rg "search"))))

(pretty-hydra-define hydra-query
  (:hint nil :color teal :quit-key "q" :title (with-faicon "search" "Engine-Mode" 1 -0.05))
  ("Query"
   (("a" engine/search-amazon "amazon")
    ("d" engine/search-duckduckgo "duckduckgo")
    ("g" engine/search-github "github")
    ("i" engine/search-google-images "google images")
    ("m" engine/search-google-maps "google maps")
    ("s" engine/search-stack-overflow "stack overflow")
    ("w" engine/search-wikipedia "wikipedia")
    ("y" engine/search-youtube "youtube"))))

(pretty-hydra-define hydra-spelling
  (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Spelling" 1 -0.05))
  ("Checker"
   (("c" langtool-correct-buffer "correction")
    ("C" langtool-check-done "clear")
    ("d" ispell-change-dictionary "dictionary")
    ("l" (message "Current language: %s (%s)" langtool-default-language ispell-current-dictionary) "language")
    ("s" my/switch-language "switch")
    ("w" wiki-summary "wiki"))
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("f" langtool-check "find"))))

(pretty-hydra-define hydra-tex
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "tex" "LaTeX" 1 -0.05))
  ("Action"
   (("g" reftex-goto-label "goto")
    ("r" reftex-query-replace-document "replace")
    ("s" counsel-rg "search")
    ("t" reftex-toc "table of content"))))

(pretty-hydra-define hydra-tool
  (:hint nil :color teal :quit-key "q" :title (with-faicon "briefcase" "Tool" 1 -0.05))
  ("Network"
   (("c" ipcalc "subnet calculator")
    ("i" ipinfo "ip info"))))

(defhydra hydra-typescript (:color blue)
  "
  ^
  ^TypeScript^          ^Do^
  ^──────────^──────────^──^───────────
  _q_ quit             _b_ back
  ^^                   _e_ errors
  ^^                   _j_ jump
  ^^                   _r_ references
  ^^                   _R_ restart
  ^^                   ^^
  "
  ("q" nil)
  ("b" tide-jump-back)
  ("e" tide-project-errors)
  ("j" tide-jump-to-definition)
  ("r" tide-references)
  ("R" tide-restart-server))

(pretty-hydra-define hydra-upload
  (:hint nil :color teal :quit-key "q" :title (with-faicon "cloud-upload" "Upload" 1 -0.05))
  ("Action"
   (("b" webpaste-paste-buffe "buffer")
    ("i" imgbb-upload "image")
    ("r" webpaste-paste-region "region"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

(defun my/dashboard-banner ()
  """Set a dashboard banner including information on package initialization
   time and garbage collections."""
  (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :config
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))
