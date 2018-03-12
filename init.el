;;; init.el --- My init file

;;; Commentary:

;;; Code:

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(after-save-hook
   (quote
    (executable-make-buffer-file-executable-if-script-p)))
 '(alert-user-configuration (quote ((nil osx-notifier nil))))
 '(auto-compile-on-load-mode t)
 '(auto-compile-on-save-mode t)
 '(blink-cursor-delay 0.3)
 '(blink-cursor-interval 0.5)
 '(calendar-date-style (quote european))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-show-numbers nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(cua-mode nil nil (cua-base))
 '(cursor-type (quote (bar . 2)))
 '(custom-magic-show-button t)
 '(custom-raised-buttons nil)
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "3d5720f488f2ed54dd4e40e9252da2912110948366a16aef503f3e9e7dfe4915" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "dd6e52a5b1180f5c8bf408764a32867e2fa86594ded78a29040cafce6a4ea808" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" default)))
 '(default-input-method (quote russian-macbook))
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(dokuwiki-login-user-name "admin")
 '(dokuwiki-xml-rpc-url "https://alebedev.site/wiki/lib/exe/xmlrpc.php")
 '(double-click-time 300)
 '(eldoc-minor-mode-string nil)
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(elpy-disable-backend-error-display nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "/usr/local/bin/flake8")
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "JAVA_HOME")))
 '(flycheck-mode-line nil)
 '(flyspell-mode-line-string nil)
 '(font-lock-global-modes (quote (not speedbar-mode)))
 '(garbage-collection-messages t)
 '(gc-cons-percentage 0.2)
 '(gc-cons-threshold 268435456)
 '(global-company-mode t)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(gnutls-trustfiles
   (quote
    ("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/usr/local/etc/libressl/cert.pem")))
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*sly-")))
 '(helm-mode t)
 '(helm-popup-tip-mode t)
 '(helm-white-buffer-regexp-list (quote ("\\`\\*sly-mrepl")))
 '(highlight-thing-delay-seconds 0.3)
 '(highlight-thing-what-thing (quote symbol))
 '(history-delete-duplicates t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines nil)
 '(inferior-lisp-program "/usr/local/bin/sbcl")
 '(isearch-allow-scroll t)
 '(load-prefer-newer t)
 '(lsp-document-sync-method (quote (quote incremental)))
 '(markdown-command "/usr/local/bin/pandoc")
 '(markdown-enable-math t)
 '(markdown-use-pandoc-style-yaml-metadata t)
 '(mbsync-status-line-re "([BCMS]: [/[:alnum:]]+)+")
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(notmuch-address-internal-completion (quote (received nil)))
 '(notmuch-after-tag-hook (quote (notmuch-hl-line-mode)))
 '(notmuch-archive-tags (quote ("-inbox +archive")))
 '(notmuch-hello-sections
   (quote
    (notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-inbox notmuch-hello-insert-footer)))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i" :search-type tree)
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a"))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-imenu-indent t)
 '(notmuch-show-logo nil)
 '(notmuch-tree-result-format
   (quote
    (("date" . "%12s  ")
     ("authors" . "%-20s")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-74s ")
     ("tags" . "(%s)"))))
 '(ns-pop-up-frames nil)
 '(org-agenda-files (quote ("~/Dropbox/emacs/org")))
 '(org-agenda-mouse-1-follows-link t)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
     (clojure . t)
     (lisp . t))))
 '(org-capture-templates
   (quote
    (("m" "Capture meeting (meetings.org)" entry
      (file "meetings.org")
      (file "meetings.template")
      :jump-to-captured t)
     ("t" "Capture task (tasks.org)" entry
      (file "tasks.org")
      "** TODO %?")
     ("i" "Capture idea (ideas.org)" entry
      (file "ideas.org")
      "* %?" :jump-to-captured t))))
 '(org-confirm-babel-evaluate nil)
 '(org-cycle-level-faces nil)
 '(org-directory "~/Dropbox/emacs/org")
 '(org-fontify-done-headline t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-id org-info org-irc org-mhe org-mouse)))
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(org-support-shift-select t)
 '(org-todo-keywords (quote ((sequence "TODO" "DONE" "CANCELLED"))))
 '(package-archive-priorities (quote (("melpa" . 100) ("elpa" . 50) ("marmalade" . 0))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-check-signature nil)
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
    (dokuwiki-mode dokuwiki ox-jira org-alert sly sly-company sly-hello-world sly-macrostep sly-named-readtables sly-quicklisp sly-repl-ansi-color helm-tramp helm-swoop ace-isearch helm helm-ag helm-cider helm-company helm-notmuch helm-pass helm-xref pyimport smart-mode-line elpy mac-pseudo-daemon deft org-brain pipenv markdown-preview-mode geiser racket-mode python-mode mbsync notmuch cheatsheet lsp-java paradox projectile lsp-ui company-lsp dockerfile-mode docker flycheck-julia auto-async-byte-compile julia-repl dired+ julia-mode emacsql-psql logstash-conf groovy-mode auto-compile racer flycheck-rust cargo rust-mode slime slime-company parinfer magithub diminish cider clojure-snippets hydra elein clj-refactor rainbow-delimiters yaml-mode solarized-theme material-theme darcula-theme flycheck highlight-thing undo-tree exec-path-from-shell ssh jabber-otr jabber writeroom-mode popwin org pandoc-mode pandoc markdown-mode multiple-cursors dracula-theme ##)))
 '(pandoc-binary "/usr/local/bin/pandoc")
 '(pandoc-process-connection-type nil)
 '(paradox-automatically-star nil)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token "cbed504f32b199377ecffc7585cd0479306de23a")
 '(password-cache-expiry 60)
 '(popwin-mode t)
 '(popwin:special-display-config
   (quote
    (("*Miniedit Help*" :position bottom :noselect t)
     (message-mode :position bottom :noselect t :tail t)
     (help-mode :width 90 :position right)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     ("*Shell Command Output*")
     ("*vc-diff*")
     ("*vc-change-log*")
     (" *undo-tree*" :width 60 :position right)
     ("*slime-apropos*")
     ("*slime-macroexpansion*")
     ("*slime-description*")
     ("*slime-compilation*" :noselect t)
     ("*slime-xref*")
     ("*Python Check*" :noselect t)
     (sldb-mode :stick t)
     (slime-repl-mode)
     (slime-connection-list-mode))))
 '(python-check-command "/usr/local/bin/flake8")
 '(python-shell-interpreter "python3")
 '(racket-program "/Applications/Racket v6.12/bin/racket")
 '(rust-format-on-save t)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scroll-margin 0)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(sql-postgres-login-params
   (quote
    ((user :default "m1st")
     server
     (database :default "m1st")
     port)))
 '(tool-bar-mode nil)
 '(void-text-area-pointer nil)
 '(windmove-wrap-around t)
 '(word-wrap t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "gray90" :foreground "#042028"))))
 '(flycheck-warning ((t (:underline (:color "yellow" :style wave)))))
 '(hi-yellow ((t (:background "#1b392d" :foreground "#a39450" :underline t))))
 '(linum ((t (:foreground "gray35" :slant normal))))
 '(scroll-bar ((t nil))))

(load-theme 'dracula t)

;; BUG: Unfortunately, auto initialization of popwin pkg did not work.
(require 'popwin)
(popwin-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings 'super)

(exec-path-from-shell-initialize)
                                        ; Prog modes

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(defun my-prog-mode-hook ()
  "Global programming mode customizations."
  (eldoc-mode t)
  (flycheck-mode t))
(add-hook 'prog-mode-hook #'my-prog-mode-hook)

;; Python
(elpy-enable)

;; Java
;; (defun my-java-mode-hook ()
;;    "Java mode hook."
;;    (require 'lsp-java)
;;    (lsp-java-enable)
;;    (require 'lsp-ui)
;;    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;    (require 'company-lsp)
;;    (push 'company-lsp company-backends))
;; (add-hook 'java-mode-hook #'my-java-mode-hook)


;; All Lisps modes

;; (eval-when-compile
;;   (defvar slime-lisp-implementations))
;; (setq slime-lisp-implementations '((sbcl ("sbcl"))))
;; (setq slime-contribs '(slime-fancy slime-company slime-repl slime-autodoc))

;; (add-hook 'slime-mode-hook
;;           (lambda ()
;;             (unless (slime-connected-p)
;;               (save-mark-and-excursion (slime)))))

;; Clojure
(defun my-lisp-mode-hook ()
  "Clojure mode hook."
  (parinfer-mode t)
  (yas-minor-mode t))

(add-hook 'clojure-mode-hook #'my-lisp-mode-hook)
(add-hook 'common-lisp-mode-hook #'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook #'my-lisp-mode-hook)
(add-hook 'clojure-mode-hook #'my-lisp-mode-hook)

;; Rust
(add-hook 'rust-mode-hook #'racer-mode)


                                        ; Misc

(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(diminish 'helm-mode)
(diminish 'highlight-thing-mode-major-mode)
(diminish 'highlight-thing-mode)
(diminish 'undo-tree-mode)
(diminish 'clj-refactor-mode)
(diminish 'company-mode)
(diminish 'visual-line-mode)

;; Keybindings
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "s-Z") 'redo)
(global-set-key [C-left] 'previous-buffer)
(global-set-key [C-right] 'next-buffer)

;; Helm
;;; Helm instead of standard for buffers/M-x/find file
(global-set-key (kbd "C-x b") #'helm-mini)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;;; Helm search with C-s and s-f
(global-set-key (kbd "C-s") #'helm-swoop)
(global-set-key (kbd "s-f") #'helm-swoop)

;; Org Mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;; init.el ends here
