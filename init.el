;;; init.el --- Andrew Braunstein's Emacs Configuration

;;; Commentary:

;; This is my portable Emacs config based on use-package.

;;; Code:

;; Set email address.
(setq user-mail-address "awbraunstein@gmail.com")

;; y/p is way better.
(setq confirm-kill-emacs 'y-or-n-p)

;; For debugging emacs.
;;(setq debug-on-error 0)

;; Use separate custom file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; For my own custom stuff.
(add-to-list 'load-path (concat user-emacs-directory "/elisp"))

;; Use separate backups area
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Save history stuff.
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; No scrollbars.
(scroll-bar-mode -1)

;; Seed the random-number generator.
(random t)

;; Saveplace (Go back to the same place you were editing in the file).
(require 'saveplace)
(setq save-place t)

;; Newline and indent everywhere.
(global-set-key (kbd "RET") 'newline-and-indent)

;; Use ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scroll to end of window before error.
(setq scroll-error-top-bottom t)

;; Automatically revert file if it changed on disk and we have no unsaved
;; changes.
(global-auto-revert-mode 1)

;; Default fill to 80 columns.
(setq-default fill-column 80)

(defun delete-whitespace-before-save ()
  "Used for prog mode to add a hook for deleting trailing whitespace on save."
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
)

(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'delete-whitespace-before-save)

;; Set up package archives.
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Use-package makes sure everything is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t) ;; Install package if necessary.

;; Global Setting.
(electric-pair-mode 0)
(electric-indent-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(setq inhibit-startup-screen 1)

(use-package smooth-scrolling
  :init (smooth-scrolling-mode t))

(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

;; Line numbers in the sidebar.
(use-package linum
  :config
  (global-linum-mode 1)
  (setq linum-format "%4d ")
  (defun turn-off-linum-mode ()
    "Turn off linenumber if called."
    (setq-local linum-format ""))
  (defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd" w) line) 'face 'linum)))
(setq linum-format 'linum-format-func))

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)
            (exec-path-from-shell-copy-env "GOPATH")))

;; Git integration.
(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
)

;; Window tiling undo.
(use-package winner
  :ensure t
  :config (winner-mode t))

; Inline syntax checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :bind (("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error))
  )

;; Nice completion
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10)
  ;;ignore .DS_Store in ido
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (ido-mode 1))

;; Better M-x
(use-package smex
  :defer t
  :bind (("M-x" . smex))
  :config
           (setq smex-history-length 100)
           (smex-initialize)
           (smex-auto-update 60))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (bind-keys :map projectile-mode-map
             ("s-D" . projectile-find-dir)
             ("s-P" . projectile-switch-project)
             ("s-F" . projectile-find-file)
             ("s-G" . projectile-grep)))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (eval '(progn (ac-set-trigger-key "TAB")
                (ac-flyspell-workaround)))
  (setq ac-auto-start nil))

;; Mode for editing web files
(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" . web-mode)
  :init
  (setq web-mode-content-types-alist
	'(("jsx"  . "\\.js[x]?\\'"))))

;; Help out with key bindings
(use-package guide-key
  :ensure t
  :config
  ;; Show on the following prefixes:
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(use-package company
  :diminish company-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil))

(defun init/go-get (package)
  "Use `go get' to install Go package PACKAGE."
  (call-process "go" nil nil nil "get" package))

(use-package go-mode
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'turn-off-auto-fill))

(use-package company-go
  :defer t
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-go))

(use-package go-guru
  :ensure t)

(use-package go-eldoc
  :defer t
  :diminish eldoc-mode
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package gotest
  :defer t
  :config
  (bind-keys :map go-test-mode-map
             ("C-c ," . go-test-current-file)
             ("C-c C-," . go-test-current-file)))

(use-package go-autocomplete
  :defer t
  :init (init/go-get "github.com/nsf/gocode"))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :ensure)

;;Go to matching parentheses
; From http://www.emacswiki.org/emacs/ParenthesisMatching#toc4
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (message "%s" last-command)
  (if (not (memq last-command
                 '(
                   set-mark
                   cua-set-mark
                   goto-match-paren
                   down-list
                   up-list
                   end-of-defun
                   beginning-of-defun
                   backward-sexp
                   forward-sexp
                   backward-up-list
                   forward-paragraph
                   backward-paragraph
                   end-of-buffer
                   beginning-of-buffer
                   backward-word
                   forward-word
                   mwheel-scroll
                   backward-word
                   forward-word
                   mouse-start-secondary
                   mouse-yank-secondary
                   mouse-secondary-save-then-kill
                   move-end-of-line
                   move-beginning-of-line
                   backward-char
                   forward-char
                   scroll-up
                   scroll-down
                   scroll-left
                   scroll-right
                   mouse-set-point
                   next-buffer
                   previous-buffer
                   previous-line
                   next-line
                   )
                 ))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))
(global-set-key (kbd "%") 'goto-match-paren)

;; Kill ring setup.
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      kill-whole-line t)

;; Immediately show the C- and M- stuff that's happening.
(setq echo-keystrokes 0.1)

;;Subword mode for navigating camelCase words.
(global-subword-mode 1)

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(global-set-key (kbd "C-c C-k") 'compile)

;; Disable C-z for 'suspend-frame' (which minimizes the frame on macOS and is annoying)
(global-unset-key "\C-z")

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Better commenting
(defun comment-uncomment-line-region (beg end)
  "Comments/uncomments the current line, or the region (if active)"
  (interactive "*r")
  (if (use-region-p)
      (comment-or-uncomment-region beg end)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
)
(global-set-key (kbd "C-c C-c") 'comment-uncomment-line-region)

;; Automatically create new directory (with prompting)
(defadvice find-file-noselect (before make-directory-maybe first (filename &optional nowarn rawfile wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (when (y-or-n-p (format "Directory %s does not exist. Create it?" dir))
          (make-directory dir))))))

;; Turn off visual bell for bug in OSX
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
(invert-face 'mode-line)
(run-with-timer 0.1 nil 'invert-face 'mode-line)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . 'ace-jump-mode)
         ("C-c C-SPC" . 'ace-jump-mode)))

(use-package github-browse-file
  :defer t)

(use-package solarized-theme
  :if (display-graphic-p)
  :custom
  (solarized-use-variable-pitch nil)
  (x-underline-at-descent-line t)
  :config
  (defun toggle-theme ()
    "Switch between Solarized variants."
    (interactive)
    (cond
     ((member 'solarized-dark custom-enabled-themes)
      (disable-theme 'solarized-dark)
      (load-theme 'solarized-light t))
     ((member 'solarized-light custom-enabled-themes)
      (disable-theme 'solarized-light)
      (load-theme 'solarized-dark t))))
  (load-theme 'solarized-dark t))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(setq-default typescript-indent-level 2)
(setq tide-format-options '(:indentSize 2 :tabSize 2))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Save history stuff.
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; No scrollbars.
(scroll-bar-mode -1)

;; Seed the random-number generator.
(random t)

;; Saveplace (Go back to the same place you were editing in the file).
(require 'saveplace)
(setq save-place t)

;; Newline and indent everywhere.
(global-set-key (kbd "RET") 'newline-and-indent)

;; Use ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scroll to end of window before error.
(setq scroll-error-top-bottom t)

;; Automatically revert file if it changed on disk and we have no unsaved
;; changes.
(global-auto-revert-mode 1)

;; Default fill to 80 columns.
(setq-default fill-column 80)

(defun delete-whitespace-before-save ()
  "Used for prog mode to add a hook for deleting trailing whitespace on save."
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
)

(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'delete-whitespace-before-save)

;; Set up package archives.
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Use-package makes sure everything is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t) ;; Install package if necessary.

;; Global Setting.
(electric-pair-mode 0)
(electric-indent-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(setq inhibit-startup-screen 1)

(use-package smooth-scrolling
  :init (smooth-scrolling-mode t))

(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

;; Line numbers in the sidebar.
(use-package linum
  :config
  (global-linum-mode 1)
  (setq linum-format "%4d ")
  (defun turn-off-linum-mode ()
    "Turn off linenumber if called."
    (setq-local linum-format ""))
  (defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd" w) line) 'face 'linum)))
(setq linum-format 'linum-format-func))

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)
            (exec-path-from-shell-copy-env "GOPATH")))

;; Git integration.
(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
)

;; Window tiling undo.
(use-package winner
  :ensure t
  :config (winner-mode t))

; Inline syntax checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :bind (("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error))
  )

;; Nice completion
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10)
  ;;ignore .DS_Store in ido
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (ido-mode 1))

;; Better M-x
(use-package smex
  :defer t
  :bind (("M-x" . smex))
  :config
           (setq smex-history-length 100)
           (smex-initialize)
           (smex-auto-update 60))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (bind-keys :map projectile-mode-map
             ("s-D" . projectile-find-dir)
             ("s-P" . projectile-switch-project)
             ("s-F" . projectile-find-file)
             ("s-G" . projectile-grep)))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (eval '(progn (ac-set-trigger-key "TAB")
                (ac-flyspell-workaround)))
  (setq ac-auto-start nil))

;; Mode for editing web files
(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" . web-mode)
  :init
  (setq web-mode-content-types-alist
	'(("jsx"  . "\\.js[x]?\\'"))))

;; Help out with key bindings
(use-package guide-key
  :ensure t
  :config
  ;; Show on the following prefixes:
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(use-package company
  :diminish company-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil))

(defun init/go-get (package)
  "Use `go get' to install Go package PACKAGE."
  (call-process "go" nil nil nil "get" package))

(use-package go-mode
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'turn-off-auto-fill))

(use-package company-go
  :defer t
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-go))

(use-package go-guru
  :ensure t)

(use-package go-eldoc
  :defer t
  :diminish eldoc-mode
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package gotest
  :defer t
  :config
  (bind-keys :map go-test-mode-map
             ("C-c ," . go-test-current-file)
             ("C-c C-," . go-test-current-file)))

(use-package go-autocomplete
  :defer t
  :init (init/go-get "github.com/nsf/gocode"))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :ensure)

;;Go to matching parentheses
; From http://www.emacswiki.org/emacs/ParenthesisMatching#toc4
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (message "%s" last-command)
  (if (not (memq last-command
                 '(
                   set-mark
                   cua-set-mark
                   goto-match-paren
                   down-list
                   up-list
                   end-of-defun
                   beginning-of-defun
                   backward-sexp
                   forward-sexp
                   backward-up-list
                   forward-paragraph
                   backward-paragraph
                   end-of-buffer
                   beginning-of-buffer
                   backward-word
                   forward-word
                   mwheel-scroll
                   backward-word
                   forward-word
                   mouse-start-secondary
                   mouse-yank-secondary
                   mouse-secondary-save-then-kill
                   move-end-of-line
                   move-beginning-of-line
                   backward-char
                   forward-char
                   scroll-up
                   scroll-down
                   scroll-left
                   scroll-right
                   mouse-set-point
                   next-buffer
                   previous-buffer
                   previous-line
                   next-line
                   )
                 ))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))
(global-set-key (kbd "%") 'goto-match-paren)

;; Kill ring setup.
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      kill-whole-line t)

;; Immediately show the C- and M- stuff that's happening.
(setq echo-keystrokes 0.1)

;;Subword mode for navigating camelCase words.
(global-subword-mode 1)

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(global-set-key (kbd "C-c C-k") 'compile)

;; Disable C-z for 'suspend-frame' (which minimizes the frame on macOS and is annoying)
(global-unset-key "\C-z")

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Better commenting
(defun comment-uncomment-line-region (beg end)
  "Comments/uncomments the current line, or the region (if active)"
  (interactive "*r")
  (if (use-region-p)
      (comment-or-uncomment-region beg end)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
)
(global-set-key (kbd "C-c C-c") 'comment-uncomment-line-region)

;; Automatically create new directory (with prompting)
(defadvice find-file-noselect (before make-directory-maybe first (filename &optional nowarn rawfile wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (when (y-or-n-p (format "Directory %s does not exist. Create it?" dir))
          (make-directory dir))))))

;; Turn off visual bell for bug in OSX
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
(invert-face 'mode-line)
(run-with-timer 0.1 nil 'invert-face 'mode-line)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . 'ace-jump-mode)
         ("C-c C-SPC" . 'ace-jump-mode)))

(use-package github-browse-file
  :defer t)

(use-package solarized-theme
  :if (display-graphic-p)
  :custom
  (solarized-use-variable-pitch nil)
  (x-underline-at-descent-line t)
  :config
  (defun toggle-theme ()
    "Switch between Solarized variants."
    (interactive)
    (cond
     ((member 'solarized-dark custom-enabled-themes)
      (disable-theme 'solarized-dark)
      (load-theme 'solarized-light t))
     ((member 'solarized-light custom-enabled-themes)
      (disable-theme 'solarized-light)
      (load-theme 'solarized-dark t))))
  (load-theme 'solarized-dark t))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Save history stuff.
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; No scrollbars.
(scroll-bar-mode -1)

;; Seed the random-number generator.
(random t)

;; Saveplace (Go back to the same place you were editing in the file).
(require 'saveplace)
(setq save-place t)

;; Newline and indent everywhere.
(global-set-key (kbd "RET") 'newline-and-indent)

;; Use ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scroll to end of window before error.
(setq scroll-error-top-bottom t)

;; Automatically revert file if it changed on disk and we have no unsaved
;; changes.
(global-auto-revert-mode 1)

;; Default fill to 80 columns.
(setq-default fill-column 80)

(defun delete-whitespace-before-save ()
  "Used for prog mode to add a hook for deleting trailing whitespace on save."
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
)

(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'delete-whitespace-before-save)

;; Set up package archives.
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Use-package makes sure everything is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t) ;; Install package if necessary.

;; Global Setting.
(electric-pair-mode 0)
(electric-indent-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(setq inhibit-startup-screen 1)

(use-package smooth-scrolling
  :init (smooth-scrolling-mode t))

(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

;; Line numbers in the sidebar.
(use-package linum
  :config
  (global-linum-mode 1)
  (setq linum-format "%4d ")
  (defun turn-off-linum-mode ()
    "Turn off linenumber if called."
    (setq-local linum-format ""))
  (defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd" w) line) 'face 'linum)))
(setq linum-format 'linum-format-func))

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)
            (exec-path-from-shell-copy-env "GOPATH")))

;; Git integration.
(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
)

;; Window tiling undo.
(use-package winner
  :ensure t
  :config (winner-mode t))

; Inline syntax checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :bind (("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error))
  )

;; Nice completion
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10)
  ;;ignore .DS_Store in ido
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (ido-mode 1))

;; Better M-x
(use-package smex
  :defer t
  :bind (("M-x" . smex))
  :config
           (setq smex-history-length 100)
           (smex-initialize)
           (smex-auto-update 60))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (bind-keys :map projectile-mode-map
             ("s-D" . projectile-find-dir)
             ("s-P" . projectile-switch-project)
             ("s-F" . projectile-find-file)
             ("s-G" . projectile-grep)))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (eval '(progn (ac-set-trigger-key "TAB")
                (ac-flyspell-workaround)))
  (setq ac-auto-start nil))

;; Mode for editing web files
(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" . web-mode)
  :init
  (setq web-mode-content-types-alist
	'(("jsx"  . "\\.js[x]?\\'"))))

;; Help out with key bindings
(use-package guide-key
  :ensure t
  :config
  ;; Show on the following prefixes:
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(use-package company
  :diminish company-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil))

(defun init/go-get (package)
  "Use `go get' to install Go package PACKAGE."
  (call-process "go" nil nil nil "get" package))

(use-package go-mode
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'turn-off-auto-fill))

(use-package company-go
  :defer t
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-go))

(use-package go-guru
  :ensure t)

(use-package go-eldoc
  :defer t
  :diminish eldoc-mode
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package gotest
  :defer t
  :config
  (bind-keys :map go-test-mode-map
             ("C-c ," . go-test-current-file)
             ("C-c C-," . go-test-current-file)))

(use-package go-autocomplete
  :defer t
  :init (init/go-get "github.com/nsf/gocode"))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :ensure)

;;Go to matching parentheses
; From http://www.emacswiki.org/emacs/ParenthesisMatching#toc4
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (message "%s" last-command)
  (if (not (memq last-command
                 '(
                   set-mark
                   cua-set-mark
                   goto-match-paren
                   down-list
                   up-list
                   end-of-defun
                   beginning-of-defun
                   backward-sexp
                   forward-sexp
                   backward-up-list
                   forward-paragraph
                   backward-paragraph
                   end-of-buffer
                   beginning-of-buffer
                   backward-word
                   forward-word
                   mwheel-scroll
                   backward-word
                   forward-word
                   mouse-start-secondary
                   mouse-yank-secondary
                   mouse-secondary-save-then-kill
                   move-end-of-line
                   move-beginning-of-line
                   backward-char
                   forward-char
                   scroll-up
                   scroll-down
                   scroll-left
                   scroll-right
                   mouse-set-point
                   next-buffer
                   previous-buffer
                   previous-line
                   next-line
                   )
                 ))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))
(global-set-key (kbd "%") 'goto-match-paren)

;; Kill ring setup.
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      kill-whole-line t)

;; Immediately show the C- and M- stuff that's happening.
(setq echo-keystrokes 0.1)

;;Subword mode for navigating camelCase words.
(global-subword-mode 1)

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(global-set-key (kbd "C-c C-k") 'compile)

;; Disable C-z for 'suspend-frame' (which minimizes the frame on macOS and is annoying)
(global-unset-key "\C-z")

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Better commenting
(defun comment-uncomment-line-region (beg end)
  "Comments/uncomments the current line, or the region (if active)"
  (interactive "*r")
  (if (use-region-p)
      (comment-or-uncomment-region beg end)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
)
(global-set-key (kbd "C-c C-c") 'comment-uncomment-line-region)

;; Automatically create new directory (with prompting)
(defadvice find-file-noselect (before make-directory-maybe first (filename &optional nowarn rawfile wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (when (y-or-n-p (format "Directory %s does not exist. Create it?" dir))
          (make-directory dir))))))

;; Turn off visual bell for bug in OSX
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
(invert-face 'mode-line)
(run-with-timer 0.1 nil 'invert-face 'mode-line)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . 'ace-jump-mode)
         ("C-c C-SPC" . 'ace-jump-mode)))

(use-package github-browse-file
  :defer t)

(use-package solarized-theme
  :if (display-graphic-p)
  :custom
  (solarized-use-variable-pitch nil)
  (x-underline-at-descent-line t)
  :config
  (defun toggle-theme ()
    "Switch between Solarized variants."
    (interactive)
    (cond
     ((member 'solarized-dark custom-enabled-themes)
      (disable-theme 'solarized-dark)
      (load-theme 'solarized-light t))
     ((member 'solarized-light custom-enabled-themes)
      (disable-theme 'solarized-light)
      (load-theme 'solarized-dark t))))
  (load-theme 'solarized-dark t))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Save history stuff.
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; No scrollbars.
(scroll-bar-mode -1)

;; Seed the random-number generator.
(random t)

;; Saveplace (Go back to the same place you were editing in the file).
(require 'saveplace)
(setq save-place t)

;; Newline and indent everywhere.
(global-set-key (kbd "RET") 'newline-and-indent)

;; Use ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scroll to end of window before error.
(setq scroll-error-top-bottom t)

;; Automatically revert file if it changed on disk and we have no unsaved
;; changes.
(global-auto-revert-mode 1)

;; Default fill to 80 columns.
(setq-default fill-column 80)

(defun delete-whitespace-before-save ()
  "Used for prog mode to add a hook for deleting trailing whitespace on save."
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
)

(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'delete-whitespace-before-save)

;; Set up package archives.
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Use-package makes sure everything is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t) ;; Install package if necessary.

;; Global Setting.
(electric-pair-mode 0)
(electric-indent-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(setq inhibit-startup-screen 1)

(use-package smooth-scrolling
  :init (smooth-scrolling-mode t))

(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

;; Line numbers in the sidebar.
(use-package linum
  :config
  (global-linum-mode 1)
  (setq linum-format "%4d ")
  (defun turn-off-linum-mode ()
    "Turn off linenumber if called."
    (setq-local linum-format ""))
  (defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd" w) line) 'face 'linum)))
(setq linum-format 'linum-format-func))

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)
            (exec-path-from-shell-copy-env "GOPATH")))

;; Git integration.
(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
)

;; Window tiling undo.
(use-package winner
  :ensure t
  :config (winner-mode t))

; Inline syntax checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :bind (("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error))
  )

;; Nice completion
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10)
  ;;ignore .DS_Store in ido
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (ido-mode 1))

;; Better M-x
(use-package smex
  :defer t
  :bind (("M-x" . smex))
  :config
           (setq smex-history-length 100)
           (smex-initialize)
           (smex-auto-update 60))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (bind-keys :map projectile-mode-map
             ("s-D" . projectile-find-dir)
             ("s-P" . projectile-switch-project)
             ("s-F" . projectile-find-file)
             ("s-G" . projectile-grep)))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (eval '(progn (ac-set-trigger-key "TAB")
                (ac-flyspell-workaround)))
  (setq ac-auto-start nil))

;; Mode for editing web files
(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" . web-mode)
  :init
  (setq web-mode-content-types-alist
	'(("jsx"  . "\\.js[x]?\\'"))))

;; Help out with key bindings
(use-package guide-key
  :ensure t
  :config
  ;; Show on the following prefixes:
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(use-package company
  :diminish company-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil))

(defun init/go-get (package)
  "Use `go get' to install Go package PACKAGE."
  (call-process "go" nil nil nil "get" package))

(use-package go-mode
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'turn-off-auto-fill))

(use-package company-go
  :defer t
  :config
  (add-hook 'go-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-go))

(use-package go-guru
  :ensure t)

(use-package go-eldoc
  :defer t
  :diminish eldoc-mode
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package gotest
  :defer t
  :config
  (bind-keys :map go-test-mode-map
             ("C-c ," . go-test-current-file)
             ("C-c C-," . go-test-current-file)))

(use-package go-autocomplete
  :defer t
  :init (init/go-get "github.com/nsf/gocode"))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :ensure)

;;Go to matching parentheses
; From http://www.emacswiki.org/emacs/ParenthesisMatching#toc4
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (message "%s" last-command)
  (if (not (memq last-command
                 '(
                   set-mark
                   cua-set-mark
                   goto-match-paren
                   down-list
                   up-list
                   end-of-defun
                   beginning-of-defun
                   backward-sexp
                   forward-sexp
                   backward-up-list
                   forward-paragraph
                   backward-paragraph
                   end-of-buffer
                   beginning-of-buffer
                   backward-word
                   forward-word
                   mwheel-scroll
                   backward-word
                   forward-word
                   mouse-start-secondary
                   mouse-yank-secondary
                   mouse-secondary-save-then-kill
                   move-end-of-line
                   move-beginning-of-line
                   backward-char
                   forward-char
                   scroll-up
                   scroll-down
                   scroll-left
                   scroll-right
                   mouse-set-point
                   next-buffer
                   previous-buffer
                   previous-line
                   next-line
                   )
                 ))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))
(global-set-key (kbd "%") 'goto-match-paren)

;; Kill ring setup.
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      kill-whole-line t)

;; Immediately show the C- and M- stuff that's happening.
(setq echo-keystrokes 0.1)

;;Subword mode for navigating camelCase words.
(global-subword-mode 1)

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(global-set-key (kbd "C-c C-k") 'compile)

;; Disable C-z for 'suspend-frame' (which minimizes the frame on macOS and is annoying)
(global-unset-key "\C-z")

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Better commenting
(defun comment-uncomment-line-region (beg end)
  "Comments/uncomments the current line, or the region (if active)"
  (interactive "*r")
  (if (use-region-p)
      (comment-or-uncomment-region beg end)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
)
(global-set-key (kbd "C-c C-c") 'comment-uncomment-line-region)

;; Automatically create new directory (with prompting)
(defadvice find-file-noselect (before make-directory-maybe first (filename &optional nowarn rawfile wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (when (y-or-n-p (format "Directory %s does not exist. Create it?" dir))
          (make-directory dir))))))

;; Turn off visual bell for bug in OSX
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
(invert-face 'mode-line)
(run-with-timer 0.1 nil 'invert-face 'mode-line)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . 'ace-jump-mode)
         ("C-c C-SPC" . 'ace-jump-mode)))

(use-package github-browse-file
  :defer t)

(use-package solarized-theme
  :if (display-graphic-p)
  :custom
  (solarized-use-variable-pitch nil)
  (x-underline-at-descent-line t)
  :config
  (defun toggle-theme ()
    "Switch between Solarized variants."
    (interactive)
    (cond
     ((member 'solarized-dark custom-enabled-themes)
      (disable-theme 'solarized-dark)
      (load-theme 'solarized-light t))
     ((member 'solarized-light custom-enabled-themes)
      (disable-theme 'solarized-light)
      (load-theme 'solarized-dark t))))
  (load-theme 'solarized-dark t))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package cython-mode
  :defer t
  :mode (("\\.pyx\\'"  . cython-mode)))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (require 'rust-mode)
  (setq rust-format-on-save t)
  (global-company-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(setq js-indent-level 2)

(server-start)

(provide 'init)

;;; init.el ends here
