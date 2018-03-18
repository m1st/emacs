;;; init.el --- My init file

;;; Commentary:

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

					; Package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

                                        ; System

(setq
 gc-cons-percentage 0.1
 gc-cons-threshold 26214400)

(setq
 ns-pop-up-frames nil
 load-prefer-newer t)

(use-package exec-path-from-shell
  :ensure
  :custom
  (exec-path-from-shell-variables (quote ("PATH" "MANPATH" "JAVA_HOME")))
  :config
  (exec-path-from-shell-initialize))

(use-package diminish
  :ensure)

(use-package paradox
  :ensure
  :custom
  (paradox-execute-asynchronously t)
  :commands (paradox-enable)
  :config
  (paradox-enable))


                                        ; Security

(setq
 password-cache-expiry 120)

(use-package gnutls
  :ensure
  :custom
  (gnutls-trustfiles
   '("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/usr/local/etc/libressl/cert.pem")))

(use-package epa
  :ensure
  :custom
  (epa-file-select-keys 'silent)
  (epa-pinentry-mode 'loopback)
  (epg-gpg-program "/usr/local/MacGPG2/bin/gpg2"))


                                        ; Look and feel
(electric-indent-mode t)
(electric-layout-mode t)
(global-visual-line-mode t)
(scroll-bar-mode 0)
(show-paren-mode t)
(tool-bar-mode 0)

(setq
 blink-cursor-delay 0.3
 isearch-allow-scroll t
 scroll-error-top-bottom t
 scroll-margin 0
 visible-bell t
 ring-bell-function 'ignore
 font-lock-global-modes '(not speedbar-mode))

(setq-default
 cursor-type '(bar . 2)
 indicate-empty-lines nil
 indicate-buffer-boundaries 'left)

(use-package solarized-theme
  :ensure
  :custom
  (custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
  (solarized-high-contrast-mode-line t)
  (solarized-scale-org-headlines nil)
  (solarized-use-variable-pitch nil)
  :config
  (load-theme 'solarized-light))


                                        ; History and sessions

(setq
 history-delete-duplicates t)

(save-place-mode t)
(savehist-mode t)


(use-package desktop
  :ensure
  :custom
  (desktop-save-mode t)
  (desktop-restore-eager t))

                                        ; Keyboard and interaction

(defalias 'yes-or-no-p 'y-or-n-p)

(cua-mode t)
(delete-selection-mode 1)

(setq-default
 indent-tabs-mode nil)

(setq
 mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))
 double-click-time 300)


(use-package undo-tree
  :ensure
  :custom
  (global-undo-tree-mode t)
  :diminish undo-tree-mode)

(use-package windmove
  :ensure
  :defer 0
  :custom
  (windmove-wrap-around t)
  :bind (("<C-left>" . previous-buffer)
         ("<C-right>" . next-buffer)
         ("<home>" . move-beginning-of-line)
         ("<end>" . move-end-of-line))
  :config
  (windmove-default-keybindings 'super))

(use-package russian-macbook
  :load-path "lisp/"
  :config
  (setq default-input-method 'russian-macbook))

(use-package helm
  :ensure
  :custom
  (helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*sly-"))
  (helm-mode t)
  (helm-popup-tip-mode t)
  (helm-white-buffer-regexp-list '("\\`\\*sly-mrepl"))
  (helm-mode-fuzzy-match t)
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-s" . helm-swoop)
         ("s-f" . helm-swoop))
  :diminish (helm-mode))

(use-package deft
  :ensure
  :custom
  (deft-auto-save-interval 5.0)
  (deft-directory "/Users/m1st/Dropbox/emacs/org")
  (deft-extensions (quote ("org" "md" "markdown" "txt" "gpg")))
  (deft-markdown-mode-title-level 1)
  (deft-new-file-format "%Y-%m-%d at %H:%M")
  (deft-recursive t)
  (deft-strip-summary-regexp "\\([
	]\\|^#\\+[[:upper:]_]+:.*$\\|:PROPERTIES:.*:END:\\)")
  (deft-use-filename-as-title t)
  :custom-face
  (deft-filter-string-error-face ((t (:inherit font-lock-warning-face :height 1.5))))
  (deft-filter-string-face ((t (:inherit font-lock-string-face :height 1.5))))
  (deft-header-face ((t (:inherit font-lock-keyword-face :weight bold :height 1.5))))
  :init
  (defun my-deft-toggle ()
    "Open and show Deft in current buffer, or bury when already in current buffer."
    (interactive)
    (if (string= (buffer-name) "*Deft*")
        (bury-buffer "*Deft*"))
    (progn
      (deft)
      (deft-refresh)))
  :bind (("<f12>" . my-deft-toggle)))

(use-package company
  :ensure
  :custom
  (company-show-numbers nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 20)
  :bind ("TAB" . company-indent-or-complete-common)
  :config
  (global-company-mode 1)
  :diminish company-mode)

(use-package hydra
  :ensure)


                                        ; Org mode

(use-package calendar
  :ensure
  :custom
  (calendar-date-style 'european)
  (calendar-week-start-day 1))

(use-package org
  :ensure
  :custom
  (org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")
  (org-agenda-files
   '("~/Dropbox/emacs/org/work.org" "~/Dropbox/emacs/org/personal.org"))
  (org-agenda-mouse-1-follows-link t)
  (org-agenda-skip-archived-trees nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-sorting-strategy
   '((agenda todo-state-up time-up priority-down category-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
  (org-agenda-time-leading-zero t)
  (org-babel-load-languages '((emacs-lisp . t) (python . t) (lisp . t)))
  (org-bullets-mode t)
  (org-confirm-babel-evaluate nil)
  (org-cycle-level-faces nil)
  (org-directory "~/Dropbox/emacs/org")
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-export-backends '(ascii html icalendar latex md))
  (org-export-with-author nil)
  (org-fontify-done-headline t)
  (org-from-is-user-regexp "\\<Alexey Lebedev\\>")
  (org-inlinetask-show-first-star t)
  (org-journal-dir "~/Dropbox/emacs/org/journal/")
  (org-journal-file-format "%Y%m%d.org.gpg")
  (org-journal-time-format "")
  (org-mobile-directory "~/Dropbox/Приложения/MobileOrg")
  (org-mobile-files '("~/Dropbox/emacs/org"))
  (org-mobile-inbox-for-pull "~/Dropbox/emacs/org/inbox.org")
  (org-modules
   '(org-bbdb org-bibtex org-crypt org-docview org-id org-info org-irc org-mhe org-mouse))
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-startup-indented t)
  (org-support-shift-select t)
  (org-time-stamp-rounding-minutes '(5 5))
  (org-todo-keywords '((sequence "TODO" "BLOCKED" "CANCELLED" "DONE")))
  :mode (("\\.org\\'" . org-mode)
         ("\\.org.gpg\\'" . org-mode))
  :init
  (defhydra hydra-org (:color red)
    "Org mode hydra"
    ("a" org-agenda-list "Org default agenda (week)")
    ("d" (org-agenda-list nil nil 1) "Org daily agenda")
    ("c" org-capture "Org capture"))
  (defun my-redo-all-agenda-buffers ()
    "If Org buffer is saved, cycle through other buffers and rebuild all Org Agendas."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'org-agenda-mode)
            (org-agenda-maybe-redo))))))

  :bind ("C-c a" . 'hydra-org/body)
  :config
  (add-hook 'after-save-hook 'my-redo-all-agenda-buffers)
  (run-with-idle-timer 30 t 'org-mobile-push)
  :diminish org-indent-mode)

(use-package pandoc
  :ensure
  :custom
  (pandoc-binary "/usr/local/bin/pandoc")
  (pandoc-process-connection-type nil))


(use-package markdown-mode
  :ensure
  :requires pandoc
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.md.gpg\\'" . markdown-mode))
  :hook (markdown-mode . pandoc-mode))


                                        ; Programming

(use-package auto-compile
  :ensure
  :custom
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))

(use-package flycheck
  :ensure
  :custom
  (flycheck-mode-line nil)
  :hook ((prog-mode) . flycheck-mode))

(use-package eldoc
  :ensure
  :hook (prog-mode-hook . eldoc-mode)
  :diminish eldoc-mode)

(use-package highlight-thing
  :ensure
  :custom
  (highlight-thing-delay-seconds 0.3)
  (highlight-thing-what-thing 'symbol)
  :hook ((prog-mode) . highlight-thing-mode)
  :diminish (highlight-thing-mode
             hi-lock-mode))

(use-package yasnippet
  :ensure
  :hook ((prog-mode) . yas-minor-mode)
  :diminish yas-minor-mode)

;; Python
(use-package elpy
  :ensure
  :custom
  (python-check-command "/usr/local/bin/flake8")
  (python-shell-interpreter "python3")
  (elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults))
  (elpy-rpc-backend "jedi")
  (elpy-rpc-python-command "python3")
  (elpy-syntax-check-command "/usr/local/bin/flake8")
  :hook (python-mode . elpy-mode))

(use-package parinfer
  :ensure
  :hook ((emacs-lisp-mode-hook . parinfer-mode)
         (common-lisp-mode-hook . parinfer-mode)
         (clojure-mode-hook . parinfer-mode)))


                                        ; Miscellaneous
(use-package alert
  :ensure
  :custom
  (alert-fade-time 10)
  (alert-user-configuration
   (quote
    ((((:title . "Agenda"))
      notifier
      ((:persistent . t)))))))

(use-package mpdel
  :ensure
  :custom
  (mpdel-prefix-key (kbd "C-c z")))

;;; init.el ends here
