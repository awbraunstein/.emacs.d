;;For Starter Kit
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings color-theme-solarized)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Load solarized dark theme
(load-theme 'solarized-dark t)

;;Add vendor/ to load path for obscure modules
(add-to-list 'load-path "~/.emacs.d/vendor")

;; (server-start)

;; Setup mac path
(when (memq window-system '(mac ns))
  (progn
    (message "Setting up Mac path")
    (unless (package-installed-p 'exec-path-from-shell)
      (package-install 'exec-path-from-shell))
    (exec-path-from-shell-initialize))
  )

;; Setup el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory "~/.emacs.d/el-get-init")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-status-file "~/.emacs.d/el-get-status.el")

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-custom-recipes/")

(el-get 'sync)

;;Smex... For better M-x completions
(require 'smex)
(setq smex-history-length 100)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;Better buffer mode
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scroll to end of window before error
(setq scroll-error-top-bottom t)

;; Ido-ubiquitous broken in M-x man, disable it
(add-to-list 'ido-ubiquitous-command-exceptions 'man)
(add-to-list 'ido-ubiquitous-command-exceptions 'w3m-goto-url)

;;ignore .DS_Store in ido
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Buffer movement by arrow keys
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;Automatically revert file if it changed on disk and we have no unsaved changes
(global-auto-revert-mode t)

(setq custom-file "~/.emacs.d/customize.el")
;;(load custom-file)

;; Set email address
(setq user-mail-address "awbraunstein@gmail.com")

;; Message beep on bell
(setq ring-bell-function (lambda () (message "*beep*")))

;; Newline and indent everywhere
(global-set-key (kbd "RET") 'newline-and-indent)

;; Reindent on yank
;; http://emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode java-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; Delete extra whitespace when killing lines
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
    Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;;Auto complete stuff
(require 'pos-tip)
(require 'auto-complete-config)
(ac-config-default)
;; Needed for compatibility with flyspell
(ac-flyspell-workaround)
(setq ac-auto-start 3)
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay .3)
(ac-set-trigger-key "TAB")
;; Make \C-n and \C-p work in autocompletion menu
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; Let's have snippets in the auto-complete dropdown
(setq-default ac-sources '(ac-source-yasnippet
                           ac-source-abbrev
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-files-in-current-dir
                           ))
;; Immediately show what is happening for C- and M-
(setq echo-keystrokes 0.1)

;;Subword mode for navigating camelCase words
(global-subword-mode 1)

;;For smooth-scrolling
(setq smooth-scroll-margin 5)

;;Haskell setup

(defun haskell-custom-setup ()
  (progn
    (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)
    (setq haskell-indent-offset 2)
    (ghc-init)
    (flymake-mode 1)

    (defun haskell-mode-stylish-buffer-custom ()
      "Apply stylish-haskell to the current buffer."
      (interactive)
      (let ((column (current-column))
            (line (line-number-at-pos))
            (buffer (current-buffer))
            (file (buffer-file-name))
            (end (buffer-size)))
        (save-buffer) ;;First save so we don't lose the changes
        ;; (shell-command ;;Now run stylish with our custom config
        ;;  (concat
        ;;   "stylish-haskell -c ~/.emacs.d/stylish-haskell-config.yaml " file)
        ;;  buffer
        ;;  "*stylish_haskell_error*")
        ;; (mark-whole-buffer)
        ;; (haskell-indent-align-guards-and-rhs 0 end) ;;Align function stuff
        ;; (goto-line line)
        ;; (goto-char (+ column (point)))
        ))
    (defadvice haskell-mode-save-buffer (before
                                         haskell-mode-stylish-before-save
                                         activate)
      (haskell-mode-stylish-buffer-custom))
    ))
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-custom-setup)
;;(setq haskell-stylish-on-save t)

;;Watch for haskell mode inheriting from prog-mode
;;Then we won't need to do this.
(add-hook 'haskell-mode-hook 'auto-fill-mode)
(add-hook 'haskell-mode-hook 'fci-mode)

(global-set-key "\C-xar" 'align-regexp)

(global-set-key (kbd "C-=") 'er/expand-region)

;; multiwebmode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (coffee-mode "<%[-=]*" "%>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "eco" "jeco"))
(multi-web-global-mode 1)

;; Haskell latex for lhs2tex
(require 'haskell-latex)

;; Wrap region
(wrap-region-mode t)
(wrap-region-global-mode t)

;; Enable GHC coding
(setq ghc-location "/Users/awbraunstein/code/ghc") ;; change as necessary

;; search withing GHC compiler code
(defun rgrep-ghc (regexp)
  (interactive (list (progn (grep-compute-defaults) (grep-read-regexp))))
  (rgrep regexp "*hs" (concat ghc-location "/compiler/")))

;; quick compile stage 2 of GHC
(defun compile-ghc ()
  (interactive)
  (compile (concat "cd " ghc-location "/ghc; make 2"))
  (set-buffer "*compilation*")
  (setq default-directory ghc-location))

(global-set-key (kbd "M-c") 'rgrep-ghc)

(defun set-compile-ghc ()
  (local-set-key (kbd "C-q") 'compile-ghc))

(add-hook 'haskell-mode-hook 'set-compile-ghc)
