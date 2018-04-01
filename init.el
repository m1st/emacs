;;; init.el --- My init file

;;; 2DO
;;; 

;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/custom.el"
      initial-buffer-choice "~/.emacs.d/init.el"
      inhibit-startup-echo-area-message t)

(kill-buffer "*scratch*")

(load-file "~/.emacs.d/private.el")

                                        ; Package management
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

                                        ; System
(setq
 gc-cons-percentage 0.1
 gc-cons-threshold (* 50 1024 1024))

(setq
 auto-save-default nil ;; Disable auto saving (#file.ext#)
 make-backup-files nil ;; Disable backup files (file.ext~)
 create-lockfiles nil ;; Disable lock files (#.file.ext)
 ns-pop-up-frames nil
 load-prefer-newer t)

(use-package exec-path-from-shell
  :ensure
  :custom
  (exec-path-from-shell-variables (quote ("PATH" "MANPATH" "JAVA_HOME")))
  :config
  (exec-path-from-shell-initialize))

(use-package delight
  :ensure)

(use-package paradox
  :ensure
  :defer 0
  :custom
  (paradox-execute-asynchronously t)
  (paradox-automatically-star nil)
  :commands (paradox-enable)
  :config
  (paradox-enable))


                                        ; Security
(setq
 password-cache-expiry 120)

(use-package gnutls
  :ensure
  :defer 0
  :custom
  (gnutls-trustfiles
   '("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/usr/local/etc/libressl/cert.pem")))

(use-package epa
  :ensure
  :defer 0
  :custom
  (epa-file-select-keys 'silent)
  (epa-pinentry-mode 'loopback)
  (epg-gpg-program "/usr/local/MacGPG2/bin/gpg2"))


                                        ; Look and feel
(electric-indent-mode t)
(electric-layout-mode t)

(global-visual-line-mode t)
(diminish 'visual-line-mode)

(scroll-bar-mode 0)
(show-paren-mode t)
(tool-bar-mode 0)

(setq help-window-select t)

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

;; In minibuffer use 'bar' cursor.
(add-hook 'minibuffer-setup-hook '(lambda () (setq cursor-type 'bar)))

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
(use-package desktop
  :ensure
  :custom
  (desktop-save-mode t)
  (desktop-restore-eager t))

(setq
 history-delete-duplicates t)

(save-place-mode t)
(savehist-mode t)


                                        ; Keyboard and interaction
(defalias 'yes-or-no-p 'y-or-n-p)

(cua-mode t)
(delete-selection-mode 1)

(setq-default
 indent-tabs-mode nil)

(setq
 mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))
 double-click-time 300)


(use-package skip-buffers
  :load-path "lisp/"
  :custom
  (sb-skippable-buffers
   '("*Messages*"
     "*Warnings*"
     "*Backtrace*"
     "*Quail Completions*"
     "*scratch*"
     "*Help*"
     "*mpd*"
     "*helm M-x*"
     "*Helm Swoop*"
     "*helm mini*"
     "*helm find files*"
     "*helm-mode-describe-function*"
     "*helm-mode-describe-variable*"
     "*helm-mode-completion-at-point*"
     "*helm-mode-epa-encrypt-file*"
     "*helm-mode-org-capture*"
     "*helm-mode-write-file*"
     "*helm-mode-xref-find-definitions*"))
  
  :config
  (skippable-buffers-mode t)
  :delight skippable-buffers-mode)

(use-package undo-tree
  :ensure
  :custom
  (global-undo-tree-mode t)
  :delight undo-tree-mode)

(use-package windmove
  :ensure
  :custom
  (windmove-wrap-around t)
  :bind (("<C-left>" . previous-buffer)
         ("<C-right>" . next-buffer)
         ("<home>" . move-beginning-of-line)
         ("<end>" . move-end-of-line)
         ("<s-left>" . windmove-left)
         ("<s-right>" . windmove-right)
         ("<s-up>" . windmove-up)
         ("<s-down>" . windmove-down)))
  
(use-package russian-macbook
  :load-path "lisp/"
  :config
  (setq default-input-method 'russian-macbook))

(use-package helm
  :ensure
  :custom
  (helm-allow-mouse t)
  (helm-boring-buffer-regexp-list '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*sly-"))
  (helm-mode t)
  (helm-popup-tip-mode t)
  (helm-white-buffer-regexp-list '("\\`\\*sly-mrepl"))
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-s" . helm-swoop)
         ("s-f" . helm-swoop))
  :config
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  :delight (helm-mode))

;; (use-package deft
;;   :ensure
;;   :commands (deft deft-refresh)
;;   :custom
;;   (deft-auto-save-interval 5.0)
;;   (deft-directory "/Users/m1st/Dropbox/emacs/org")
;;   (deft-extensions (quote ("org" "md" "markdown" "txt" "gpg")))
;;   (deft-markdown-mode-title-level 1)
;;   (deft-new-file-format "%Y-%m-%d at %H:%M")
;;   (deft-recursive t)
;;   (deft-strip-summary-regexp "\\([
;; 	]\\|^#\\+[[:upper:]_]+:.*$\\|:PROPERTIES:.*:END:\\)")
;;   (deft-use-filename-as-title t)
;;   :custom-face
;;   (deft-filter-string-error-face ((t (:inherit font-lock-warning-face :height 1.5))))
;;   (deft-filter-string-face ((t (:inherit font-lock-string-face :height 1.5))))
;;   (deft-header-face ((t (:inherit font-lock-keyword-face :weight bold :height 1.5))))
;;   :config
;;   (defun my-deft-toggle ()
;;     "Open and show Deft in current buffer, or bury when already in current buffer."
;;     (interactive)
;;     (if (string= (buffer-name) "*Deft*")
;;         (bury-buffer "*Deft*"))
;;     (progn
;;       (deft)
;;       (deft-refresh)))
;;   :bind (("<f12>" . my-deft-toggle)))

(use-package ag
  :ensure)

(use-package company
  :ensure
  :commands (company-indent-or-complete-common)
  :custom
  (company-show-numbers nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 20)
  (global-company-mode 1)
  :bind ("TAB" . company-indent-or-complete-common)
  :delight company-mode)

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
  (org-agenda-files "~/Dropbox/emacs/org/agendas.org")
  (org-agenda-mouse-1-follows-link t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-sorting-strategy
   '((agenda todo-state-up time-up priority-down category-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
  (org-agenda-time-leading-zero t)
  (org-babel-load-languages '((emacs-lisp . t) (python . t) (lisp . t)))
  (org-capture-templates
   '(("j" "Journal entry" entry
      (file "~/Dropbox/emacs/org/journal.org.gpg")
      "** %T %^{prompt}"
      :empty-lines-before 1)))
  (org-directory "~/Dropbox/emacs/org/")
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-fontify-done-headline t)
  (org-from-is-user-regexp "\\<Alexey Lebedev\\>")
  (org-mobile-directory "~/Dropbox/emacs/mobileorg")
  (org-mobile-files '("~/Dropbox/emacs/org"))
  (org-mobile-inbox-for-pull "~/Dropbox/emacs/org/mobile.org")
  (org-modules
   '(org-bbdb org-bibtex org-crypt org-docview org-info org-id org-mhe org-mouse))
  (org-pretty-entities t)
  (org-src-fontify-natively t)
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
    ("j" org-journal-new-entry "New journal entry")
    ("t" org-todo-list "Todo global list")
    ("c" org-capture "Org capture")
    ("p" org-mobile-push "Push data for mobile")
    ("l" org-mobile-pull "Pull data from mobile")
    ("o p" (find-file "~/Dropbox/emacs/org/personal.org") "personal.org")
    ("o w" (find-file "~/Dropbox/emacs/org/work.org") "work.org"))
  :bind ("C-c a" . 'hydra-org/body)
  :config
  ;; (defun my-redo-all-agenda-buffers ()
  ;;   "If Org buffer is saved, cycle through other buffers and rebuild all Org Agendas."
  ;;   (interactive)
  ;;   (when (derived-mode-p 'org-mode)
  ;;     (dolist (buffer (buffer-list))
  ;;       (with-current-buffer buffer
  ;;         (when (derived-mode-p 'org-agenda-mode)
  ;;           (org-agenda-maybe-redo))))
  ;;     (org-mobile-push)))
  (defun my-before-save-hook ()
    "Before Org buffer is saved, align all tags."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (org-align-all-tags)))
  ;; (add-hook 'after-save-hook 'my-redo-all-agenda-buffers)
  (add-hook 'before-save-hook 'my-before-save-hook)
  (org-bullets-mode t))
  ;; (run-with-idle-timer 30 t 'org-mobile-push))

(use-package org-bullets
  :ensure
  :custom
  (org-bullets-bullet-list
   '(
     "✚"))
     ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
     ;;; Small
     ;; ► • ★ ▸
  :hook (org-mode . org-bullets-mode))

(use-package pandoc
  :ensure
  :custom
  (pandoc-binary "/usr/local/bin/pandoc"))


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
  ;; (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))

(use-package flycheck
  :ensure
  :custom
  (flycheck-mode-line nil)
  :hook (prog-mode . flycheck-mode)
  :delight flycheck-mode)

(use-package eldoc
  :ensure
  :hook (prog-mode . eldoc-mode)
  :delight eldoc-mode)

(use-package highlight-thing
  :ensure
  :custom
  (highlight-thing-delay-seconds 0.3)
  (highlight-thing-what-thing 'symbol)
  :hook (prog-mode . highlight-thing-mode)
  :delight (highlight-thing-mode hi-lock-mode))

(use-package yasnippet
  :ensure
  :hook ((python-mode) . yas-minor-mode)
  :delight yas-minor-mode)

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
  :hook (python . elpy-mode))

(use-package parinfer
  :ensure
  :hook ((emacs-lisp-mode common-lisp-mode clojure-mode) . parinfer-mode))


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

;; MPD controls

(use-package mpdel
  :ensure
  :config
  (libmpdel-refresh-status)
  (defun my-mpdel-info-string ()
    (libmpdel-ensure-connection)
    (libmpdel-refresh-status)
    (let* ((hydra--song (libmpdel-current-song))
           (hydra--album (libmpdel-album hydra--song))
           (hydra--song-name (libmpdel-entity-name hydra--song))
           (hydra--song-artist (libmpdel-artist-name hydra--song))
           (hydra--play-state (libmpdel-play-state))
           (hydra--album-name (libmpdel-album-name hydra--song))
           (hydra--volume (libmpdel-volume)))
      (format "[%s] %s - %s [%s], v: %s" hydra--play-state hydra--song-artist hydra--song-name hydra--album-name hydra--volume)))
    
  ;; Hydra
  (defhydra hydra-mpdel
    (:color blue :hint nil)
    "
%s(my-mpdel-info-string)

^mpd.el^                 ^Playback^                          ^Volume^
---------------------------------------------------------------------------------------
_m_: show playlist       _p_: play/pause                     _<up>_,_<down>_: volume up/down
_n_: show library        _<left>_,_<right>_: prev/next song
_s_: show current song   _<S-left>_,_<S-right>_: +/-10 secs
_q_: exit hydra
"
    ("m" mpdel-playlist-open)
    ("n" mpdel-nav-open-artists)
    ("s" mpdel-song-open)
    ("q" nil)
    ("p" libmpdel-playback-play-pause :color red)
    ("<left>" libmpdel-playback-previous :color red)
    ("<right>" libmpdel-playback-next :color red)
    ("<S-left>" mpdel-song-normal-decrement :color red)
    ("<S-right>" mpdel-song-normal-increment :color red)
    ("<up>"
     (let
         ((new-volume (number-to-string (min (+ (string-to-number (libmpdel-volume)) 5) 100))))
       (libmpdel-playback-set-volume new-volume)
       (message "Volume: %s" new-volume))
     :color red)
    ("<down>"
     (let
         ((new-volume (number-to-string (max (- (string-to-number (libmpdel-volume)) 5) 0))))
       (libmpdel-playback-set-volume new-volume)
       (message "Volume: %s" new-volume))
     :color red))
  :bind ("C-c z" . 'hydra-mpdel/body)
  :hook ((libmpdel-current-song-changed libmpdel-player-changed) . (lambda () (message "%s" (my-mpdel-info-string))))
  :delight mpdel-mode)


(use-package w3m
  :ensure
  :config
  (setq browse-url-browser-function 'w3m-browse-url))


(use-package elfeed
  :ensure
  :custom
  (elfeed-search-filter "@6-months-ago"))


(use-package elfeed-goodies
  :ensure
  :after (elfeed)
  :custom
  (elfeed-feeds '("https://habrahabr.ru/rss/feed/posts/cd09ef3c1d9619887b6520f57c331b54/"
                  "http://www.opennet.ru/opennews/opennews_6_full.rss"
                  "http://www.opennet.ru/opennews/opennews_mini_full.rss"
                  "http://linux.org.ru/rss.jsp"))
  (elfeed-goodies/powerline-default-separator 'bar)
  :config
  (elfeed-goodies/setup))

;;; init.el ends here
