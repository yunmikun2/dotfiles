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

(put 'use-package 'lisp-indent-function 1)

(use-package use-package-core
  :custom
  (use-package-enable-imenu-support t))

(use-package emacs
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (linum-mode 1)
  (setq-default cursor-type 'bar)
  (setq-default frame-title-format '("Emacs: %f"))
  (column-number-mode 1)
  (setq-default show-trailing-whitespace t)
  (my/set-system-font "Iosevka Light-12")
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (setq inhibit-startup-screen t)
  (delete-selection-mode 1)
  (setq ring-bell-function 'ignore)
  ;; I don't know what it is.
  (put 'upcase-region 'disabled nil)
  ;; Backups.
  (setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
  :bind
  ("C-;" . 'my/comment-or-uncomment)
  ("C-x k" . 'kill-current-buffer)
  ("C-c z" . 'my/zoom-font)
  ("C-x n f" . 'fold-this)
  ("C-x n u" . 'fold-this-unfold-at-point))

(use-package string-inflection
  :ensure t
  :bind
  ("C-c i" . 'string-inflection-cycle))

(use-package org
  :config
  (setq org-src-fontify-natively t)
  (set-face-attribute 'org-level-1 nil :height 150)
  (set-face-attribute 'org-level-2 nil :height 140)
  (set-face-attribute 'org-level-3 nil :height 130)
  (set-face-attribute 'org-level-4 nil :height 125)
  (set-face-attribute 'org-level-5 nil :height 120)
  (set-face-attribute 'org-level-6 nil :height 120))

(use-package markdown-mode
  :ensure t
  :config
  (set-face-attribute 'markdown-header-face-1 nil :height 150)
  (set-face-attribute 'markdown-header-face-2 nil :height 140)
  (set-face-attribute 'markdown-header-face-3 nil :height 130)
  (set-face-attribute 'markdown-header-face-4 nil :height 125)
  (set-face-attribute 'markdown-header-face-5 nil :height 120)
  (set-face-attribute 'markdown-header-face-6 nil :height 120)
  (define-key markdown-mode-map (kbd "C-c C-c v") 'my/switch-markdown-mode))

(use-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package :ensure t)

;; :diminish keyword
(use-package diminish :ensure t)

;; :bind keyword
(use-package bind-key :ensure t)

;; :quelpa keyword
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package :ensure t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1)
  (set-face-attribute 'sp-pair-overlay-face nil :background "#EEE"))

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode 1))

(use-package sublime-themes
  :ensure t
  :config
  (load-theme 'mccarthy t))

(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-map)
  (sublimity-map-set-delay nil)
  (setq sublimity-map-size 20)
  (setq sublimity-map-fraction 0.3)
  (setq sublimity-map-text-scale -7)
  :bind
  ([f9] . 'sublimity-mode))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  (set-face-attribute 'mode-line nil :background "#335EA8")
  (set-face-attribute 'mode-line-inactive nil :background "#9B9C97")
  ;; File position.
  (set-face-attribute 'mood-line-unimportant nil :foreground "#EEE")
  ;; Not modified git line
  (set-face-attribute 'mood-line-status-grayed-out nil :foreground "#EEE")
  ;; Modified git line.
  (set-face-attribute 'mood-line-status-info nil :foreground "#EEE"))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paren
  :ensure t
  :config
  (show-paren-mode 1))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq dumb-jump-selector 'ivy))

(use-package ace-window
  :ensure t
  :bind
  (("M-o" . 'ace-window)
   ("C-x o" . 'ace-window)))

(use-package neotree
  :ensure t
  :bind
  ([f8] . 'neotree-toggle)
  :config
  (setq neo-window-position 'right)
  (setq neo-window-width 60))

(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))

(use-package ranger
  :ensure t
  :bind
  ("C-x C-d" . 'deer)
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-show-hidden t))

(use-package auto-complete
  :ensure t
  :config
  (auto-complete-mode 1))

(use-package company
  :ensure t
  :config
  (company--idle-delay)
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("<backtab>" . 'company-complete-common-or-cycle))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-root-files-functions
        '(projectile-root-local
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring))
  :bind
  ("C-'" . 'projectile-previous-project-buffer)
  ("C-\"" . 'projectile-next-project-buffer))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package multiple-cursors
  :ensure t
  :config
  :bind
  (("C-." . 'mc/mark-next-like-this)
   ("C->" . 'mc/unmark-next-like-this)
   ("C-," . 'mc/mark-previous-like-this)
   ("C-<" . 'mc/unmark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . 'swiper-isearch)
  ("C-M-s" . 'swiper-isearch-thing-at-point))

(use-package counsel
  :ensure t
  :config
  (setq projectile-project-switch-action 'counsel-projectile-switch-to-buffer)
  (setq projectile-project-search-path '("~/wrk/"))
  (define-key projectile-mode-map (kbd "C-x i") 'counsel-imenu)
  :bind
  ("C-r" . 'counsel-projectile-ag)
  ("C-x p" . 'counsel-projectile-switch-project)
  ("C-x C-b" . 'counsel-switch-buffer)
  ("C-x b" . 'counsel-projectile-switch-to-buffer)
  ("C-x f" . 'counsel-projectile-find-file)
  ("C-x C-f" . 'counsel-find-file))

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

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-format-hook
            (lambda ()
              (let ((args (list "--dot-formatter"
                                (concat (projectile-project-root)
                                        ".formatter.exs"))))
                (if (projectile-project-p)
                    (setq elixir-format-arguments args)))))
  ;; (format-all-mode)
  ;; (add-hook 'elixir-mode-hook
  ;;           (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (add-hook 'elixir-mode
            (lambda ()
              (alchemist-mode-hook)
              (dumb-jump-mode)))
  (define-key elixir-mode-map (kbd "C-c a f c v") 'my/ex-find-controller-for-view)
  (define-key elixir-mode-map (kbd "C-c a f v c") 'my/ex-find-view-for-controller)
  (define-key elixir-mode-map (kbd "C-c a f c s") 'my/ex-find-controller-for-swagger)
  (define-key elixir-mode-map (kbd "C-c a f s c") 'my/ex-find-swagger-for-controller)
)

(use-package alchemist
  :ensure t
  ;; :config
  ;; Use a different keybinding prefix than C-c a; default: (kbd "C-c a")
  ;; (setq alchemist-key-command-prefix (kbd "C-c ,"))
  )

(use-package ruby-mode
  :ensure t
  :config
  (add-hook 'ruby-mode
            (lambda ()
              (company-mode 1))))

(use-package elisp-mode
  :config
  (setq lisp-body-indent 2)
  (add-hook 'emacs-lisp-mode
            (lambda ()
              (set (make-local-variable 'lisp-indent-function)
                   'common-lisp-indent-function)
              (put 'lambda 'lisp-indent-function 'defun)
              (put 'while 'lisp-indent-function 1)
              (put 'if 'lisp-indent-function 1))))

(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode
            (lambda ()
              (compilation-minor-mode)))
  (put 'downcase-region 'disabled nil))

(use-package cc-mode
  :ensure t
  :config
  (setq-default c-default-style "k&r"
                c-basic-offset 2
                indent-tabs-mode nil)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-toggle-auto-state 1))))

(use-package python-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (run-python)
              (require 'jedi)
              (setq jedi:complete-on-dot 1)
              (jedi:setup))))

(use-package ispell
  :ensure t
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US"))

(use-package js
  :ensure t
  :config
  (setq js-indent-level 2))

(defun my/sh-mode-indentation ()
  (interactive)
  (setq sh-basic-offset 2
        sh-indentation 2))

(use-package sh-script
  :ensure t
  :config
  (add-hook 'sh-mode-hook 'my/sh-mode-indentation))

;;
;; All configuration is broken apart into those files.
;;
