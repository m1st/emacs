(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-library "russian-macbook")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-file-name t)
 '(blink-cursor-delay 0.3)
 '(blink-cursor-interval 0.5)
 '(calendar-date-style (quote european))
 '(calendar-week-start-day 1)
 '(cider-enlighten-mode nil)
 '(cider-repl-use-pretty-printing t)
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-nxml company-css company-semantic company-clang company-xcode company-cmake company-capf company-files
		  (company-dabbrev-code company-gtags company-etags company-keywords company-jedi)
		  company-dabbrev)))
 '(company-show-numbers t)
 '(custom-magic-show-button t)
 '(custom-raised-buttons nil)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "dd6e52a5b1180f5c8bf408764a32867e2fa86594ded78a29040cafce6a4ea808" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" default)))
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(double-click-time 300)
 '(eldoc-minor-mode-string "Eldoc")
 '(elpy-mode-hook
   (quote
    (lambda nil
      (progn
	(setq-local flymake-start-syntax-check-on-newline t)
	(setq-local flymake-no-changes-timeout 0.5)))))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "/usr/local/bin/python3")
 '(elpy-syntax-check-command "/usr/local/bin/flake8")
 '(ess-sas-shell-buffer-remote-host "sasrtdm65")
 '(ess-sas-submit-command "/opt/sas/sashome/SASFoundation/9.4/sas")
 '(focus-follows-mouse nil)
 '(font-lock-global-modes (quote (not speedbar-mode)))
 '(gc-cons-percentage 0.2)
 '(gc-cons-threshold 80000000)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(gnutls-trustfiles
   (quote
    ("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/usr/local/etc/libressl/cert.pem")))
 '(helm-allow-mouse t)
 '(helm-cider-mode t)
 '(helm-google-suggest-search-url "https://google.com/search?ie=utf-8&oe=utf-8&q=%s")
 '(helm-google-url "https://google.ru/search?ie=UTF-8&oe=UTF-8&q=%s")
 '(helm-mode t)
 '(helm-split-window-in-side-p t)
 '(highlight-thing-delay-seconds 0.3)
 '(highlight-thing-what-thing (quote symbol))
 '(history-delete-duplicates t)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(line-number-mode t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(markdown-enable-math t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(ns-antialias-text t)
 '(ns-pop-up-frames nil)
 '(org-agenda-files (quote ("~/Dropbox/emacs/org")))
 '(org-agenda-mouse-1-follows-link t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t))))
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
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1,T2A" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("" "hyperref" nil))))
 '(org-latex-packages-alist (quote (("english,russian" "babel" nil))))
 '(org-latex-tables-centered nil)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-mouse org-rmail org-w3m)))
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(org-todo-keywords (quote ((sequence "TODO" "DONE" "CANCELLED"))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
    (slime slime-company osx-plist ox-tiddly parinfer magithub diminish smartparens cider clojure-snippets hydra elein helm-google helm-cider clj-refactor rainbow-delimiters yaml-mode company-jedi python solarized-theme material-theme darcula-theme flycheck highlight-thing undo-tree exec-path-from-shell ess ssh jabber-otr jabber elpy writeroom-mode popwin org pandoc-mode pandoc markdown-mode multiple-cursors dracula-theme ##)))
 '(pandoc-binary "/usr/local/bin/pandoc")
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
 '(python-shell-completion-native-enable nil)
 '(python-shell-completion-native-try-output-timeout 3.0)
 '(python-shell-interpreter "python3")
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(scroll-margin 5)
 '(show-paren-mode t)
 '(slime-kill-without-query-p t)
 '(sml/mode-width (quote full))
 '(sml/theme (quote powerline))
 '(sp-base-key-bindings nil)
 '(telephone-line-evil-use-short-tag t)
 '(telephone-line-mode t)
 '(telephone-line-primary-left-separator (quote telephone-line-cubed-left))
 '(telephone-line-primary-right-separator (quote telephone-line-cubed-right))
 '(telephone-line-secondary-left-separator (quote telephone-line-cubed-hollow-left))
 '(telephone-line-secondary-right-separator (quote telephone-line-cubed-hollow-right))
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(void-text-area-pointer nil)
 '(windmove-wrap-around t)
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warnline ((t (:underline (:color "orange" :style wave)))))
 '(hi-yellow ((t (:background "#1b392d" :foreground "#a39450" :underline t))))
 '(linum ((t (:foreground "gray35" :slant normal))))
 '(scroll-bar ((t nil))))

(load-theme 'darcula t)

;; BUG: Unfortunately, auto initialization of popwin pkg did not work.
(require 'popwin)
(popwin-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'markdown-mode-hook 'pandoc-mode)

; Auto rebuild agenda on Org mode save.
(defun my-redo-all-agenda-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
	(let ((w (get-buffer-window buffer t)) (w0 (selected-window)))
	  (when w
	    (select-window w)
	    (org-agenda-redo)
	    (select-window w0)
	    ))))))

(add-hook 'org-mode-hook
	  (lambda()
	    (add-hook 'after-save-hook 'my-redo-all-agenda-buffers nil nil)))

(windmove-default-keybindings 'super)

(exec-path-from-shell-initialize)

; Python
;; ELPY
(add-hook 'elpy-mode-hook 'flycheck-mode)
(elpy-enable)
; Treat underscore as a part of the word.
(modify-syntax-entry ?_ "w")

; Clojure
(global-company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (paredit-mode 0)
  (parinfer-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

; SLIME/SBCL
(defun my-lisp-mode-hook ()
  (paredit-mode 0)
  (parinfer-mode 1)
  (setq slime-lisp-implementations
	'((sbcl ("sbcl" "--core" "/Users/m1st/sbcl.core-for-slime"))))
  (setq slime-contribs '(slime-fancy slime-company))
  (unless (slime-connected-p)
    (save-excursion (slime))))
(add-hook 'lisp-mode-hook #'my-lisp-mode-hook)

; Misc
(add-hook 'prog-mode-hook 'highlight-thing-mode)
(global-set-key (kbd "M-x") 'helm-M-x)

(diminish 'helm-mode)
(diminish 'highlight-thing-mode)
(diminish 'undo-tree-mode)
(diminish 'clj-refactor-mode)
(diminish 'smartparens-mode)

;; Keybindings
(global-set-key (kbd "s-Z") 'redo)
(global-set-key [C-left] 'previous-buffer)
(global-set-key [C-right] 'next-buffer)
; Org Mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
; Lang switcher with Karabiner and Caps Lock
(global-set-key (kbd "<f12>") (kbd "C-\\"))
