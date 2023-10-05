(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs-backups/")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

(setq xref-search-program #'ripgrep)

(use-package emacs
  :custom
  (inhibit-startup-message t)
  (initial-scratch-message nil)
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (global-hl-line-mode +1)

  (column-number-mode)

  (global-display-line-numbers-mode t)
  (dolist (mode '(term-mode-hook
		  shell-mode-hook
		  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-medium t))

(use-package emacs
  :config
  (set-face-attribute 'default nil :font "Source Code Pro" :height 140))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package general
 :demand
 :config
 (general-create-definer personal/leader-key
  :keymaps 'override
  :prefix "S-SPC")
 (general-create-definer personal/refactor
  :keymaps 'override
  :prefix "M-RET"))

(use-package hydra)

(use-package which-key
 :custom (which-key-idle-delay 0.3)
 :config (which-key-mode))

(use-package helpful
  :general
  ([remap describe-key] #'helpful-key)
  ([remap describe-function] #'helpful-function)
  ([remap describe-variable] #'helpful-variable)
  ([remap describe-command] #'helpful-command))

(use-package magit
 :custom (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package treemacs-magit
  :after (magit treemacs))

(use-package forge :after (magit))

(use-package magit-gitflow
  :hook 'magit-mode-hook (turn-on-magit-gitflow)
  :after (magit))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode t))

(use-package git-gutter
  :config (global-git-gutter-mode +1))

(use-package emacs
  :custom
  (isearch-wrap-pause 'no-ding "Disable the pause and the ding when search wraps around"))

(use-package avy
  :bind ("C-'" . avy-goto-char-timer)
  :custom (avy-setup-default))

(use-package treemacs
  :bind ("C-x t t" . treemacs)
  :config
  (treemacs-display-current-project-exclusively)
  (treemacs-project-follow-mode))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t "Vertico list cycles at the end")
  (read-extended-command-predicate #'command-completion-default-include-p "Hide commands not valid for the current mode")
  (enable-recursive-minibuffers t "Minibuffers can use minibuffers"))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (corfu-separator ?\s)
  (completion-styles '(orderless))
  :init
  (global-corfu-mode))

(use-package cape)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package embark
  :general
  ("C-." #'embark-act)
  ("C-;" #'embark-dwim))

(use-package embark-consult
  :after (embark consult))

(use-package iedit
  :bind ("C-#" . iedit-mode))

(use-package org
  :custom (org-ellipsis " ➤")
  (org-log-done 'time)
  (org-agenda-start-with-log-mode t)
  (org-duration-format (quote h:mm)))

(use-package org-bullets
:after org
:hook (org-mode . org-bullets-mode))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package ox-reveal)

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  (org-mode . org-download-enable))

(use-package org-inline-anim
  :custom (org-inline-anim-loop t)
  :hook (org-mode . org-inline-anim-mode))

(use-package emacs
  :config
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (show-paren-mode 1))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package rainbow-delimiters
 :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
 :hook (prog-mode . enable-paredit-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(setq treesit-language-source-alist '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
				      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
				      (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git" nil "src"))))

(use-package docker)

(use-package clojure-mode
  :hook ((clojure-mode . eglot-ensure)
	 (clojurec-mode . eglot-ensure)
	 (clojurescript-mode . eglot-ensure))
  :config (setq eldoc-idle-delay 2))

(use-package clojure-mode-extra-font-locking
  :after (clojure-mode))

(use-package flycheck-clj-kondo
  :after (clojure-mode))

(use-package clj-refactor
  :hook ((clojure-mode . clj-refactor-mode)
	 (clojurec-mode . clj-refactor-mode)
	 (clojurescript-mode . clj-refactor-mode))
  :config
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "M-RET"))

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :custom
  (cider-eval-toplevel-inside-comment-form t)
  (clojure-toplevel-inside-comment-form t))

;; Leverage an existing cider nrepl connection to evaluate portal.api functions
;; and map them to convenient key bindings.

;; def portal to the dev namespace to allow dereferencing via @dev/portal
(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
    "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(setq personal/portal-keymap (make-sparse-keymap))
(global-set-key (kbd "S-<SPC> p") (cons "portal" personal/portal-keymap))
(global-set-key (kbd "S-<SPC> p o") '("open" . portal.api/open))
(global-set-key (kbd "S-<SPC> p c") '("clear" . portal.api/clear))

(defcustom personal/portal-tap-viewers '(":portal.viewer/inspector"
					 ":portal.viewer/pprint"
					 ":portal.viewer/table"
					 ":portal.viewer/tree"
					 ":portal.viewer/hiccup"
					 ":portal.viewer/tree")
  "List of viewers to be prompted when `C-u M-x personal/cider-tap-last-sexp`")

(defun personal/cider-tap-last-sexp (&optional default-viewer)
  "Evaluate and tap the expression preceding point.
   If invoked with default-viewer, add this as metadata.
  If invoked with a single prefix argument, prompt for the viewer using the values defined in `personal/portal-tap-viewers`"
  (interactive (list (when (consp current-prefix-arg)
		       (completing-read "Default Viewer: " personal/portal-tap-viewers))))
  (cider-interactive-eval
   (if default-viewer
       (concat "(tap> (vary-meta "
	       (apply #'buffer-substring-no-properties (cider-last-sexp 'bounds))
	       " merge {:portal.viewer/default "
	       default-viewer
	       "}))")
     (concat "(tap> " (apply #'buffer-substring-no-properties (cider-last-sexp 'bounds)) ")"))))

(global-set-key (kbd "C-S-<return>") 'personal/cider-tap-last-sexp)

(defun personal/get-namespace-for-symbol-dict(sym-dict)
  "Get the namespace from the provided `nrepl-dict`"
  (nrepl-dict-get sym-dict "ns"))

(defun personal/get-symbol-name-for-symbol-dict(symbol-dict)
  "Get the symbol-name from the provided `nrepl-dict`"
  (nrepl-dict-get symbol-dict "name"))

(defun personal/get-namespaced-symbol-for-symbol-dict(symbol-dict)
  "Get the namespaced symbol name from the provided `nrepl-dict`"
  (concat (personal/get-namespace-for-symbol-dict symbol-dict)
	  "/"
	  (personal/get-symbol-name-for-symbol-dict symbol-dict)))

(defun personal/get-project-relative-file-path-for-current-file()
  "Get the path to the current file, relative to the project root"
  (file-relative-name (buffer-file-name) (clojure-project-root-path)))

(defun personal/get-current-line-in-relative-file-path()
  "Get the path to the current file, relative to the project root followed by the line number"
  (concat (personal/get-project-relative-file-path-for-current-file)
	  ":"
	  (number-to-string (current-line))))

(defun personal/kill-reference-to-symbol()
   "Kill a reference to the current namespaced symbol."
  (interactive)
  (let ((symbol-dict (cider-var-info (cider-symbol-at-point))))
    (kill-new (personal/get-namespaced-symbol-for-symbol-dict symbol-dict))))

(defun personal/kill-reference-to-line()
  "Kill a reference to the current line in the file."
  (interactive)
  (kill-new (personal/get-current-line-in-relative-file-path)))

(defun personal/kill-reference-to-namespace ()
  "Kill a reference to the current namespace"
  (interactive)
  (kill-new (substring-no-properties (clojure-find-ns))))

(setq personal/kill-reference-keymap (make-sparse-keymap))
(global-set-key (kbd "S-<SPC> k") (cons "kill-reference" personal/kill-reference-keymap))
(global-set-key (kbd "S-<SPC> k s") '("kill-reference-to-symbol" . personal/kill-reference-to-symbol))
(global-set-key (kbd "S-<SPC> k n") '("kill-reference-to-ns" . personal/kill-reference-to-namespace))
(global-set-key (kbd "S-<SPC> k l") '("kill-reference-to-line" . personal/kill-reference-to-line))

(use-package clj-deps-new)

(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)
	 (clojurec-mode . aggressive-indent-mode)
	 (clojurescript-mode . aggressive-indent-mode)))

(use-package clojure-snippets
  :after yasnippet clojure-mode)

(use-package sly)

(defun uuid-create ()
"Return a newly generated UUID. This uses a simple hashing of variable data."
(let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
		      (user-uid)
		      (emacs-pid)
		      (system-name)
		      (user-full-name)
		      user-mail-address
		      (current-time)
		      (emacs-uptime)
		      (garbage-collect)
		      (random)
		      (recent-keys)))))
  (format "%s-%s-3%s-%s-%s"
	  (substring s 0 8)
	  (substring s 8 12)
	  (substring s 13 16)
	  (substring s 16 20)
	  (substring s 20 32))))

(defun uuid-insert ()
"Inserts a new UUID at the point."
(interactive)
(insert (uuid-create)))

(use-package nov
 :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package rfc-mode
  :custom
  (rfc-mode-directory (expand-file-name "~/.local/rfc")))

(use-package adoc-mode)

(general-def
  "M-Z" 'zap-up-to-char
  "M-i" 'imenu)

(setq switch-to-buffer-in-dedicated-window t)
(add-to-list 'display-buffer-alist
	     '("\\*.*-?(?repl\\|e?shell)?.*\\*" display-buffer-in-side-window
	       (side . bottom)
	       (slot . 0)
	       (window-height . 0.3)
	       (window . root)
	       (dedicated . t)))

(use-package restclient)

(use-package terraform-mode
:custom (terraform-indent-level 4))
