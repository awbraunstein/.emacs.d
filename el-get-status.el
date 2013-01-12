((ProofGeneral status "installed" recipe
               (:name ProofGeneral :website "http://proofgeneral.inf.ed.ac.uk/" :description "Proof General is a generic front-end for proof assistants (also known as interactive theorem provers)" :type http-tar :options
                      ("xzf")
                      :url "http://proofgeneral.inf.ed.ac.uk/releases/ProofGeneral-4.2.tgz" :build
                      ("cd ProofGeneral && make clean" "cd ProofGeneral && make compile")
                      :build/darwin
                      `("cd ProofGeneral && make clean" ,(concat "cd ProofGeneral && make compile EMACS=" el-get-emacs))
                      :load
                      ("ProofGeneral/generic/proof-site.el")
                      :info "./ProofGeneral/doc/"))
 (auctex status "installed" recipe
         (:name auctex :website "http://www.gnu.org/software/auctex/" :description "AUCTeX is an extensible package for writing and formatting TeX files in GNU Emacs and XEmacs. It supports many different TeX macro packages, including AMS-TeX, LaTeX, Texinfo, ConTeXt, and docTeX (dtx files)." :type cvs :module "auctex" :url ":pserver:anonymous@cvs.sv.gnu.org:/sources/auctex" :build
                `(("./autogen.sh")
                  ("./configure" "--without-texmf-dir" "--with-lispdir=`pwd`" ,(concat "--with-emacs=" el-get-emacs))
                  "make")
                :load-path
                ("." "preview")
                :load
                ("tex-site.el" "preview/preview-latex.el")
                :info "doc"))
 (auto-complete status "installed" recipe
                (:name auto-complete :website "http://cx4a.org/software/auto-complete/" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
                       (popup fuzzy)))
 (buffer-move status "installed" recipe
              (:name buffer-move :description "Swap buffers without typing C-x b on each window" :type emacswiki :features buffer-move))
 (coffee-mode status "installed" recipe
              (:name coffee-mode :website "http://ozmm.org/posts/coffee_mode.html" :description "Emacs Major Mode for CoffeeScript" :type github :pkgname "defunkt/coffee-mode" :features coffee-mode :post-init
                     (progn
                       (add-to-list 'auto-mode-alist
                                    '("\\.coffee$" . coffee-mode))
                       (add-to-list 'auto-mode-alist
                                    '("Cakefile" . coffee-mode))
                       (setq coffee-js-mode 'javascript-mode))))
 (expand-region status "installed" recipe
                (:name expand-region :type github :pkgname "magnars/expand-region.el" :description "Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want." :website "https://github.com/magnars/expand-region.el#readme" :features expand-region))
 (flymake-cursor status "installed" recipe
                 (:name flymake-cursor :type github :pkgname "illusori/emacs-flymake-cursor" :description "displays flymake error msg in minibuffer after delay (illusori/github)" :website "http://github.com/illusori/emacs-flymake-cursor"))
 (fuzzy status "installed" recipe
        (:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (gh status "installed" recipe
     (:name gh :type github :pkgname "sigma/gh.el" :depends
            (pcache logito)
            :description "Github API client libraries" :website "http://github.com/sigma/gh.el"))
 (ghc-mod status "installed" recipe
          (:name ghc-mod :description "Happy Haskell programming" :type github :pkgname "kazu-yamamoto/ghc-mod" :load-path "elisp"))
 (gist status "installed" recipe
       (:name gist :type github :pkgname "defunkt/gist.el" :depends
              (gh tabulated-list)
              :description "Emacs integration for gist.github.com" :website "http://github.com/defunkt/gist.el"))
 (go-mode status "installed" recipe
          (:name go-mode :description "Major mode for the Go programming language" :type http :url "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip" :localname "go-mode.el"))
 (haskell-latex status "installed" recipe
                (:name haskell-latex :description "multi-mode for working with haskell and latex" :depends multi-mode :type http :url "http://www.loveshack.ukfsn.org/emacs/haskell-latex.el"))
 (haskell-mode status "installed" recipe
               (:name haskell-mode :description "A Haskell editing mode" :type github :pkgname "haskell/haskell-mode" :load "haskell-site-file.el" :post-init
                      (progn
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                        (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
 (inf-ruby status "installed" recipe
           (:name inf-ruby :type http :description "Inferior Ruby Mode - ruby process in a buffer." :url "http://bugs.ruby-lang.org/projects/ruby-trunk/repository/raw/misc/inf-ruby.el"))
 (logito status "installed" recipe
         (:name logito :type github :pkgname "sigma/logito" :description "logging library for Emacs" :website "http://github.com/sigma/logito"))
 (markdown-mode status "installed" recipe
                (:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :type git :url "git://jblevins.org/git/markdown-mode.git" :before
                       (add-to-list 'auto-mode-alist
                                    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (multi-mode status "installed" recipe
             (:name multi-mode :description "A multi major mode for emacs" :type http :url "http://www.loveshack.ukfsn.org/emacs/multi-mode.el"))
 (multi-web-mode status "installed" recipe
                 (:name "multi-web-mode" :description "Multi Web Mode is a minor mode which makes web editing in Emacs much easier" :type github :pkgname "fgallina/multi-web-mode"))
 (package status "installed" recipe
          (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin 24 :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
                 (progn
                   (setq package-user-dir
                         (expand-file-name
                          (convert-standard-filename
                           (concat
                            (file-name-as-directory default-directory)
                            "elpa")))
                         package-directory-list
                         (list
                          (file-name-as-directory package-user-dir)
                          "/usr/share/emacs/site-lisp/elpa/"))
                   (make-directory package-user-dir t)
                   (unless
                       (boundp 'package-subdirectory-regexp)
                     (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
                   (setq package-archives
                         '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (pcache status "installed" recipe
         (:name pcache :type github :pkgname "sigma/pcache" :description "persistent caching for Emacs" :website "http://github.com/sigma/pcache"))
 (popup status "installed" recipe
        (:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (pos-tip status "installed" recipe
          (:name pos-tip :description "Show tooltip at point" :type emacswiki))
 (tabulated-list status "installed" recipe
                 (:name tabulated-list :type github :pkgname "sigma/tabulated-list.el" :description "generic major mode for tabulated lists." :website "http://github.com/sigma/tabulated-list.el"))
 (wrap-region status "installed" recipe
              (:name wrap-region :description "Wrap text with punctation or tag" :type elpa :prepare
                     (progn
                       (autoload 'wrap-region-mode "wrap-region" nil t)
                       (autoload 'turn-on-wrap-region-mode "wrap-region" nil t)
                       (autoload 'turn-off-wrap-region-mode "wrap-region" nil t)
                       (autoload 'wrap-region-global-mode "wrap-region" nil t))))
 (yasnippet status "installed" recipe
            (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :pre-init
                   (unless
                       (or
                        (boundp 'yas/snippet-dirs)
                        (get 'yas/snippet-dirs 'customized-value))
                     (setq yas/snippet-dirs
                           (list
                            (concat el-get-dir
                                    (file-name-as-directory "yasnippet")
                                    "snippets"))))
                   :post-init
                   (put 'yas/snippet-dirs 'standard-value
                        (list
                         (list 'quote
                               (list
                                (concat el-get-dir
                                        (file-name-as-directory "yasnippet")
                                        "snippets")))))
                   :compile nil :submodule nil)))
