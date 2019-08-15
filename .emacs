;;; -*- lexical-binding: t; -*-
;;; Emacs config.
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("marmalade" . "https://marmalade-repo.org/packages/")
   ;; '("melpa" . "http://melpa.milkbox.net/packages/")
   '("melpa" . "https://melpa.org/packages/")
   t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defun my/open-notes ()
  (interactive)
  (find-file "~/notes.org"))

(defun my/word-wrap ()
  (interactive)
  (visual-line-mode t))

(defun my/set-system-font (font)
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font)
  (set-face-attribute 'default nil :font font)
  (set-frame-font font nil t))

(defun my/make-emacs-transparent ()
  (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
  (add-to-list 'default-frame-alist '(alpha . (98 . 98))))

(defun my/set-buffer-font (font size)
  (interactive "sFont: \nnSize: ")
  "Sets the specified font in current buffer"
  (setq buffer-face-mode-face `(:family ,font :height ,(* size 10)))
  (buffer-face-mode))

;; Set default window size.
(defun my/set-default-window-size (width height top left)
  (if (display-graphic-p)
      (progn
        (setq initial-frame-alist
              `(
                (tool-bar-lines . 0)
                (width . ,width)
                (heigth . ,height)
                (top . ,top)
                (left . ,left)))
        (setq default-frame-alist
              '(
                (tool-bar-lines . 0)
                (width . ,width)
                (heigth . ,heigh)
                (top . ,top)
                (left . ,left))))
    (progn
      (setq initial-frame-alist '( (tool-bar-lines . 0)))
      (setq default-frame-alist '( (tool-bar-lines . 0))))))

(defun my/switch-markdown-mode ()
  (interactive)
  (if (equal major-mode 'markdown-mode)
    (markdown-view-mode)
    (progn
      (markdown-mode)
      (read-only-mode -1))))

(setq *my/font-size-is-normal* t)

(defun my/zoom-font ()
  (interactive)
  (if *my/font-size-is-normal*
      (progn
       (setq buffer-face-mode-face `(:height 60))
       (setq *my/font-size-is-normal* nil)
       (buffer-face-mode))
    (progn
       (setq buffer-face-mode-face `(:height 120))
       (setq *my/font-size-is-normal* t)
       (buffer-face-mode))))

(defun my/comment-or-uncomment ()
  (interactive)
  (if (region-active-p)
    (funcall-interactively 'comment-or-uncomment-region
                           (region-beginning)
                           (region-end))
    (funcall-interactively 'comment-line 1)))

(defun my/projectile-find-file-for (project-file)
  (find-file project-file)
  (funcall-interactively 'counsel-projectile-find-file))

(use-package use-package-core
  :custom (use-package-enable-imenu-support t))

(use-package cus-edit
  :custom (custom-file null-device "Don't store customizations"))

(use-package gcmh
  :ensure t
  :init (gcmh-mode 1))

(use-package system-packages
  :ensure t
  :custom (system-packages-noconfirm t))

(use-package use-package-ensure-system-package :ensure t)

;; :diminish keyword
(use-package diminish :ensure t)

;; :bind keyword
(use-package bind-key :ensure t)

;; :quelpa keyword
(use-package quelpa
  :ensure t
  :defer t
  :custom (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package :ensure t)

(use-package emacs
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (linum-mode 1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (my/set-system-font "Iosevka Light-12")
  (mapcar (lambda (x) (put x 'disabled nil))
          '(narrow-to-page
            narrow-to-region
            scroll-left
            downcase-region
            upcase-region))
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; (add-to-list 'load-path "~/.emacs.d/mlibs/")
  :hook (after-init . (lambda () (scroll-bar-mode -1)))
  :custom
  (inhibit-startup-screen t)
  (ring-bell-function 'ignore)
  (backup-directory-alist '((".*" . "~/.emacs.d/backups")) "Backups")
  ;; (auto-save-file-name-transforms '((".*" . "~/.emacs.d/backups")) "Hash-trash")
  (cursor-type 'bar)
  (frame-title-format '("Emacs: %f"))
  (show-trailing-whitespace t)
  :bind
  ("C-;" . 'my/comment-or-uncomment)
  ("C-x k" . 'kill-current-buffer)
  ("C-c z" . 'my/zoom-font)
  ("C-x n f" . 'fold-this)
  ("C-x n u" . 'fold-this-unfold-at-point)
  ("C-c m n" . 'my/open-notes))

(use-package string-inflection
  :ensure t
  :bind ("C-c i" . 'string-inflection-cycle))

(use-package org
  :ensure t
  :custom (org-src-fontify-natively t)
  :custom-face
  (org-level-1 ((nil (:height 150))))
  (org-level-2 ((nil (:height 140))))
  (org-level-3 ((nil (:height 130))))
  (org-level-4 ((nil (:height 125))))
  (org-level-5 ((nil (:height 120))))
  (org-level-6 ((nil (:height 120))))
  (org-block ((nil (:background "#FFF"))))
  ;; Why doesn't it work?
  :config (set-face-font 'org-default "Roboto"))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "/usr/bin/pandoc")
  :custom-face
  (markdown-header-face-1 ((nil (:height 150))))
  (markdown-header-face-2 ((nil (:height 140))))
  (markdown-header-face-3 ((nil (:height 130))))
  (markdown-header-face-4 ((nil (:height 125))))
  (markdown-header-face-5 ((nil (:height 120))))
  (markdown-header-face-6 ((nil (:height 120))))
  :config
  (set-face-font 'markdown-inline-code-face "Iosevka Light")
  (set-face-font 'markdown-pre-face "Iosevka Light")
  :bind
  (:map markdown-mode-map
        ("C-c C-c v" . 'my/switch-markdown-mode))
  :hook
  (markdown-mode . (lambda () (my/set-buffer-font "Roboto" 12))))

(use-package auto-fill-mode
  :custom (fill-collumn 80)
  :hook text-mode markdown-mode org-mode)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  :custom-face (sp-pair-overlay-face (nil ((:background "#EEE")))))

(use-package smooth-scroll
  :ensure t
  :config (smooth-scroll-mode 1))

(use-package sublime-themes
  :ensure t
  :config (load-theme 'mccarthy t))

(use-package sublimity
  :ensure t
  :bind ([f9] . 'sublimity-mode))

(use-package sublimity-map
  :config (sublimity-map-set-delay nil)
  :custom
  (sublimity-map-size 20)
  (sublimity-map-fraction 0.3)
  (sublimity-map-text-scale -7))

(use-package mood-line
  :ensure t
  :config (mood-line-mode)
  :custom-face
  (mode-line ((nil (:background "#335EA8"))))
  (mode-line-inactive ((nil (:background "#9B9C97"))))
  ;; File position.
  (mood-line-unimportant ((nil (:foreground "#EEE"))))
  ;; Not modified git line
  (mood-line-status-grayed-out ((nil (:foreground "#EEE"))))
  ;; Modified git line.
  (mood-line-status-info ((nil (:foreground "#EEE")))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paren
  :ensure t
  :config (show-paren-mode 1))

(use-package ivy
  :ensure t
  :config (ivy-mode 1)
  :custom (dumb-jump-selector 'ivy))

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . 'ace-window)
  ("C-x o" . 'ace-window))

(use-package which-key
  :ensure t
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.05)
  :config
  (mapcar (lambda (key) (add-to-list 'which-key-replacement-alist key))
          '((("TAB" . nil) . ("↹" . nil))
            (("RET" . nil) . ("⏎" . nil))
            (("DEL" . nil) . ("⇤" . nil))
            (("SPC" . nil) . ("␣" . nil))))
  (which-key-mode)
  :bind ("C-h <f5>" . 'which-key-C-h-dispatch))

(use-package neotree
  :ensure t
  :bind ([f8] . 'neotree-toggle)
  :custom
  (neo-window-position 'right)
  (neo-window-width 60))

(use-package tramp
  :ensure t
  :custom (tramp-default-method "ssh"))

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode +1)
  :custom
  (git-gutter:update-interval 2)
  ;; (git-gutter:ask-p nil "Don't ask on revert.")
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  :custom-face
  (git-gutter:modified ((nil (:background "#C38418"))))
  (git-gutter:added ((nil (:background "#335EA8"))))
  (git-gutter:deleted ((nil (:background "#F22C40"))))
  :bind
  ("C-c g n" . git-gutter:next-hunk)
  ("C-c g p" . git-gutter:previous-hunk)
  ("C-c g r" . git-gutter:revert-hunk)
  ("C-c g c" . git-gutter:stage-hunk)
  ("C-c g s" . git-gutter:popup-hunk)
  ("C-c g m" . git-gutter:mark-hunk))

(use-package magit
  :ensure t
  :bind
  (:map magit-file-mode-map
        ("C-c g g" . 'magit-dispatch)))

(use-package ranger
  :ensure t
  :bind ("C-x C-d" . 'deer)
  :custom
  (ranger-cleanup-eagerly t)
  (ranger-show-hidden t)
  :config (ranger-override-dired-mode t))

(use-package auto-complete
  :ensure t
  :config (auto-complete-mode 1))

(use-package company
  :ensure t
  :config (company--idle-delay)
  :hook (after-init . global-company-mode)
  :bind ("<backtab>" . 'company-complete-common-or-cycle))

(use-package company-quickhelp
  :ensure t
  :config (company-quickhelp-mode 1))

(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :custom
  (projectile-project-root-files-functions
   '(projectile-root-local projectile-root-top-down projectile-root-bottom-up
     projectile-root-top-down-recurring)
   "Projectile project searcher.")
  :bind
  ("C-'" . 'projectile-previous-project-buffer)
  ("C-\"" . 'projectile-next-project-buffer)
  (:map projectile-mode-map
        ("C-c p" . 'projectile-command-map)))

(use-package reverse-im
  :ensure t
  :config (reverse-im-activate "russian-computer"))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-." . 'mc/mark-next-like-this)
  ("C->" . 'mc/unmark-next-like-this)
  ("C-," . 'mc/mark-previous-like-this)
  ("C-<" . 'mc/unmark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . 'swiper-isearch)
  ("C-M-s" . 'swiper-isearch-thing-at-point))

(use-package counsel
  :ensure t
  :custom
  (projectile-project-switch-action 'counsel-projectile-switch-to-buffer)
  (projectile-project-search-path '("~/wrk/"))
  :bind
  ("C-r" . 'counsel-projectile-ag)
  ("C-x p" . 'counsel-projectile-switch-project)
  ("C-x C-b" . 'counsel-switch-buffer)
  ("C-x b" . 'counsel-projectile-switch-to-buffer)
  ("C-x f" . 'counsel-projectile-find-file)
  ("C-x C-f" . 'counsel-find-file)
  (:map projectile-mode-map
        ("C-x i" . 'counsel-imenu)))

(defun my/ex-gen-migration (migration-name)
  (interactive "sMigration name: ")
  (let* ((timestamp (format-time-string "%Y%m%d%H%M%S" (current-time)))
         (file-name (concat (replace-regexp-in-string " " "_" migration-name)))
         (file-path (concat (projectile-project-root)
                            "priv/repo/migrations/"
                            timestamp "_" file-name ".exs"))
         (mix-file-name (concat (projectile-project-root) "mix.exs"))
         (project-file
          (with-temp-buffer
            (insert-file-contents-literally mix-file-name)
            (buffer-string)))
         (mix-first-line (car (split-string project-file "\n")))
         (mix-module-name (replace-regexp-in-string "defmodule " ""
                                                    mix-first-line))
         (current-project-name (car (split-string mix-module-name "\\.")))
         (module-name (concat current-project-name
                              ".Repo.Migrations."
                              (string-inflection-pascal-case-function
                               file-name)))
         (module-content (concat "defmodule "
                                 module-name
                                 " do\n"
                                 "  use Ecto.Migration\n\n"
                                 "  def change do\n"
                                 "  end\n"
                                 "end\n")))
    (append-to-file module-content nil file-path)
    (funcall-interactively 'find-file file-path)))

(defun my/ex-swagger-for-controller ()
  (interactive)
  (let* ((first-line (car (split-string (buffer-string) "\n")))
         (module-name
          (replace-regexp-in-string "^defmodule " ""
                                    (replace-regexp-in-string " do$" ""
                                                              first-line)))
         (swagger-module-name (replace-regexp-in-string "Controller$" "Swagger"
                                                        module-name))
         (m-buffer-file-name (replace-regexp-in-string "controller.ex"
                                                       "swagger.ex"
                                                       (buffer-file-name)))
         (swagger-file-path (replace-regexp-in-string "\\/controllers\\/"
                                                      "/swagger/"
                                                      m-buffer-file-name))
         (swagger-content (concat "defmodule " swagger-module-name " do\n"
                                  "  @moduledoc false\n\n"
                                  "  use PhoenixSwagger\n\n"
                                  "end\n")))
    (append-to-file swagger-content nil swagger-file-path)
    (funcall-interactively 'find-file swagger-file-path)))

(defun my/ex--open-something-with-replaced (from to &optional from2 to2)
  (let* ((pre-new (replace-regexp-in-string from to (buffer-file-name)))
         (new (if (and from2 to2)
                  (replace-regexp-in-string from2 to2 pre-new)
                pre-new)))
    (funcall-interactively 'find-file new)))

(defun my/ex-find-view-for-controller ()
  (interactive)
  (my/ex--open-something-with-replaced "controller" "view"))

(defun my/ex-find-controller-for-view ()
  (interactive)
  (my/ex--open-something-with-replaced "view" "controller"))

(defun my/ex-find-swagger-for-controller ()
  (interactive)
  (my/ex--open-something-with-replaced "\\/controllers\\/"
                                       "/swagger/"
                                       "controller"
                                       "swagger"))

(defun my/ex-find-controller-for-swagger ()
  (interactive)
  (my/ex--open-something-with-replaced "\\/swagger\\/"
                                       "/controllers/"
                                       "swagger"
                                       "controller"))

(use-package chevengur-imenu-mode
  :quelpa
  (chevengur-imenu-mode :fetcher github
                        :repo "yunmikun2/chevengur-imenu-mode")
  :hook elixir-mode)

(defun my/prog/elixir-format-hook ()
  (if (projectile-project-p)
      (setq elixir-format-arguments
            (list "--dot-formatter"
                  (concat (locate-dominating-file buffer-file-name
                                                  ".formatter.exs")
                          ".formatter.exs")))
    (setq elixir-format-arguments nil)))

(use-package alchemist
  :ensure t
  :hook (elixir-mode . alchemist-mode))

(use-package elixir-mode
  :ensure t
  :hook
  (elixir-format . my/prog/elixir-format-hook)
  (elixir-mode . dumb-jump-mode)
  :bind
  (:map elixir-mode-map
        ("C-c a f c v" . 'my/ex-find-controller-for-view)
        ("C-c a f v c" . 'my/ex-find-view-for-controller)
        ("C-c a f c s" . 'my/ex-find-controller-for-swagger)
        ("C-c a f s c" . 'my/ex-find-swagger-for-controller)
        ("C-c a d" . 'elixir-mode-open-docs-stable)))

(use-package pug-mode
  :ensure t
  :custom (pug-tab-width 2))

(use-package ruby-mode
  :ensure t
  :config
  :hook (ruby-mode . company-mode))

;; (use-package inf-ruby
;;  :hook
;;  ;; automatically switch from common ruby compilation modes
;;  ;; to interact with a debugger
;;  (compilation-filter . inf-ruby-auto-enter)
;;  ;; required to use binding.pry or byebug
;;  (after-init . inf-ruby-switch-setup))

;; (use-package robe
;;  :after (company)
;;  :hook
;;  (ruby-mode . robe-mode)
;;  :config
;;  (add-to-list 'company-backends 'company-robe)
;;  :delight "robe")

;; (use-package rubocop
;;  :after (robe)
;;  :hook
;;  (ruby-mode . rubocop-mode)
;;  :delight "rcop")

;; (use-package bundler
;;  :after general
;;  :config
;;  (nmap 'ruby-mode-map
;;    :prefix my/leader
;;    "b i" 'bundle-install
;;    "b c" 'bundle-console
;;    "b o" 'bundle-outdated
;;    "b u" 'bundle-update
;;    "b e" 'bundle-exec))

;; (use-package rbenv
;;  :commands
;;  (global-rbenv-mode)
;;  :preface
;;  (defun my/rbenv/modeline (current-ruby)
;;    (append
;;     '(" ruby [")
;;     (list (propertize current-ruby 'face 'rbenv-active-ruby-face))
;;     '("]")))
;;  :hook
;;  (ruby-mode . rbenv-use-corresponding)
;;  :init
;;  (setq rbenv-modeline-function 'my/rbenv/modeline)
;;  :config
;;  (global-rbenv-mode)
;;  (nmap 'ruby-mode-map
;;    :prefix "C-c R"
;;    "c" 'rbenv-use-corresponding
;;    "u" 'rbenv-use))

;; (use-package rake
;;  :after (general projectile)
;;  :init
;;  (setq rake-completion-system projectile-completion-system)
;;  :config
;;  (nmap 'ruby-mode-map
;;    :prefix my/leader
;;    "r" 'rake))

;; (use-package rspec-mode)

;; (use-package projectile-rails
;;  :after projectile
;;  :commands
;;  (projectile-rails-global-mode)
;;  :init
;;  (setq
;;   projectile-rails-vanilla-command "bin/rails"
;;   projectile-rails-spring-command "bin/spring"
;;   projectile-rails-zeus-command "bin/zeus")
;;  :config
;;  (projectile-rails-global-mode)
;;  :diminish)

(defun my/prog/emacs-lisp-mode-hook ()
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (put 'lambda 'lisp-indent-function 'defun)
  (put 'while 'lisp-indent-function 1)
  (put 'if 'lisp-indent-function 1)
  (put 'use-package 'lisp-indent-function 1)
  (put 'define-derived-mode 'lisp-indent-function 1))

(use-package elisp-mode
  :custom (lisp-body-indent 2)
  :hook (emacs-lisp-mode . my/prog/emacs-lisp-mode-hook)
  :bind ("C-c e e" . 'eval-region))

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . compilation-minor-mode))

(use-package cc-mode
  :ensure t
  :custom
  (c-default-style "k&r")
  (c-basic-offset 2)
  (indent-tabs-mode nil)
  :hook (c-mode-common-hook . c-toggle-auto-state))

(use-package jedi
  :ensure t
  :custom (jedi:complete-on-dot 1))

(use-package python-mode
  :ensure t
  :hook (python-mode . jedi:ac-setup))

(use-package ispell
  :ensure t
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_US"))

(use-package js
  :ensure t
  :custom (js-indent-level 2))

(use-package sh-script
  :ensure t
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))
