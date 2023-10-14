;; -*- lexical-binding: t -*-

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

(use-package ef-themes)

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

(personal/leader-key "<SPC> t" '("complete-tag" . complete-tag))
(use-package cape
  :config
  (personal/leader-key "<SPC> p" '("completion-at-point" . completion-at-point)) ;; capf
  (personal/leader-key "<SPC> t" '("complete-tag" . complete-tag))        ;; etags
  (personal/leader-key "<SPC> d" '("cape-dabbrev" . cape-dabbrev))        ;; or dabbrev-completion
  (personal/leader-key "<SPC> h" '("cape-history" . cape-history))
  (personal/leader-key "<SPC> f" '("cape-file" . cape-file))
  (personal/leader-key "<SPC> k" '("cape-keyword" . cape-keyword))
  (personal/leader-key "<SPC> s" '("cape-elisp-symbol" . cape-elisp-symbol))
  (personal/leader-key "<SPC> e" '("cape-elisp-block" . cape-elisp-block))
  (personal/leader-key "<SPC> a" '("cape-abbrev" . cape-abbrev))
  (personal/leader-key "<SPC> l" '("cape-line" . cape-line))
  (personal/leader-key "<SPC> w" '("cape-dict" . cape-dict))
  (personal/leader-key "<SPC> :" '("cape-emoji" . cape-emoji))
  (personal/leader-key "<SPC> \\" '("cape-tex" . cape-tex))
  (personal/leader-key "<SPC> _" '("cape-tex" . cape-tex))
  (personal/leader-key "<SPC> ^" '("cape-tex" . cape-tex))
  (personal/leader-key "<SPC> &" '("cape-sgml" . cape-sgml))
  (personal/leader-key "<SPC> r" '("cape-rfc1345" . cape-rfc1345)))

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
  (org-duration-format (quote h:mm))
  (custom-set-faces
   '(org-level-1 ((t (:height 1.5))))
   '(org-level-2 ((t (:height 1.4))))
   '(org-level-3 ((t (:height 1.3))))))

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

(use-package org-roam
  :config (org-roam-db-autosync-mode))

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
 :hook (clojure-mode . enable-paredit-mode)
 (emacs-lisp-mode . enable-paredit-mode)
 (lisp-mode . enable-paredit-mode))

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

(setq python-indent-offset 4)
(setq python-shell-interpreter "python3")

(use-package elpy
:init
(elpy-enable)
:config
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(setq elpy-rpc-virtualenv-path 'current))

(use-package pyvenv
:after elpy
:config
(setenv "WORKON_HOME" (expand-file-name "~/.local/share/virtualenvs/")))

(use-package blacken
:hook (python-mode . blacken-mode))

(setq flycheck-python-flake8-executable "/usr/bin/flake8")

(use-package rust-mode
:hook
((rust-mode . (lambda () (setq indent-tabs-mode nil)))
 (rust-mode . racer-mode)
 (rust-mode . flycheck-rust-setup)))

(use-package racer
:after rust-mode
:hook
(racer-mode . eldoc-mode))

(use-package flycheck-rust
:after rust-mode
:config
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
:after rust-mode
:hook
(rust-mode . cargo-minor-mode))

(use-package web-mode
:straight t
:mode (("\\.html?\\'" . web-mode)
       ("\\.css\\'" . web-mode)
       ("\\.js\\'" . web-mode))
:config
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t))

(use-package emmet-mode
:straight t
:hook (web-mode css-mode sgml-mode))

(use-package rainbow-mode
:straight t
:hook (web-mode css-mode))

(use-package js2-mode
  :straight t
  :mode "\\.js\\'")

(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package prettier-js
  :straight t
  :hook ((web-mode js2-mode typescript-mode) . prettier-js-mode))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq flycheck-javascript-eslint-executable "/usr/bin/eslint"))

(use-package indium
:straight t
:hook (js2-mode . indium-interaction-mode))

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

(defun my/eshell-git-repo-status ()
"Return a concise repo status string."
(string-trim (shell-command-to-string
  	      "git status --porcelain=v1 | awk 'BEGIN {staged=0; modified=0; untracked=0} /^M/ {modified++} /^M / {staged++} /^??/ {untracked++} END {sep=\"\"; if (staged > 0) {printf \"S\" staged; sep=\"|\"} if (modified > 0) {printf sep \"M\" modified; sep=\"|\"} if (untracked > 0) {printf sep \"U\" untracked} print \"\"}'"))
)

(defun my/eshell-git-prompt ()
  "Return the current branch and status for the eshell prompt"
  (let ((branch (magit-get-current-branch))
      (status (my/eshell-git-repo-status)))
  (when branch
    (concat "|"
            branch
            (if (not (string-empty-p status)) (concat "[" status "]"))))))

(defun my/eshell-prompt ()
  "Return the full eshell prompt string"
  (concat "[" user-login-name "@" (system-name) (my/eshell-git-prompt) "] " (eshell/pwd) " $ "))

(setq eshell-prompt-function 'my/eshell-prompt)

(defun my-read-only-eshell-prompt ()
    (let ((prompt (my/eshell-prompt)))
      (add-text-properties 0 (length prompt) '(read-only t) prompt)
      prompt))

    (setq eshell-prompt-function 'my-read-only-eshell-prompt)

    (defun my-read-only-eshell-output ()
      (let ((inhibit-read-only t)
  	    (beg (eshell-beginning-of-output))
  	    (end (eshell-end-of-output)))
        (put-text-property beg end 'read-only t)))

;    (add-hook 'eshell-output-filter-functions 'my-read-only-eshell-output)

(use-package xterm-color
:ensure t
:after eshell-mode
:config
;; This will set `xterm-color-filter' as the filter for comint processes. This includes eshell,
;; but also other modes like shell-mode or compilation-mode.
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)

;; For eshell specifically
(setq eshell-output-filter-functions
      (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
)

(add-hook 'eshell-before-prompt-hook
          (lambda ()
          (setq xterm-color-preserve-properties t)
  	(setenv "TERM" "xterm-256color")))

(defun my/hiragana-conversion ()
(interactive)
(let* ((word (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (thing-at-point 'word t)))
       (hiragana (shell-command-to-string (format "echo \"%s\" | kakasi -i \"utf-8\" -f -JH -KH" word))))
  (message "Hiragana: %s" hiragana)))

(when (member "Noto Color Emoji" (font-family-list))
(set-fontset-font
  t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

(use-package elfeed)

(use-package elfeed-org
  :config (elfeed-org)
    (setq rmh-elfeed-org-files (list (expand-file-name "~/.config/emacs/elfeed.org"))))

(defun my/get-selected-region-or-prompt ()
"Get the selected region's content or prompt the user if no region is selected."
(if (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))
  (read-string "URL: ")))

(defun my/eww-open-readable ()
"Open URL in readable mode."
(interactive)
(letrec ((nonce (lambda ()
                  (unwind-protect
                      (eww-readable)
                    (remove-hook 'eww-after-render-hook nonce)))))
  (add-hook 'eww-after-render-hook nonce))
(let ((selected-url (my/get-selected-region-or-prompt)))
  (eww selected-url)))

(use-package breadcrumb
  :config (breadcrumb-mode))
