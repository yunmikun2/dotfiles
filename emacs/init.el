;;; -*- lexical-binding: t; -*-
;;; Local Variables:
;;; eval: (add-hook 'after-save-hook #'byte-compile-current-file nil t)
;;; End:
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

;; Vanya's packages.
(add-to-list 'package-archives '("flashbacks" . "https://elpa.flashbacks.club/"))
(add-to-list 'package-unsigned-archives "flashbacks")

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
  :defer t
  :custom (custom-file null-device "Don't store customizations"))

(use-package gcmh
  :ensure t
  :defer t
  :init (gcmh-mode 1))

(use-package system-packages
  :ensure t
  :defer t
  :custom (system-packages-noconfirm t))

(use-package use-package-ensure-system-package
  :ensure t)

;; :diminish keyword
(use-package diminish
  :ensure t
  :defer t)

;; :bind keyword
(use-package bind-key
  :ensure t
  :defer t)

;; :quelpa keyword
(use-package quelpa
  :ensure t
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
  :bind
  ("C-;" . 'my/comment-or-uncomment)
  ("C-x k" . 'kill-current-buffer)
  ("C-c z" . 'my/zoom-font)
  ("C-x n f" . 'fold-this)
  ("C-x n u" . 'fold-this-unfold-at-point)
  ("C-c n n" . 'my/open-notes))

(use-package string-inflection
  :ensure t
  :bind ("C-c i" . 'string-inflection-cycle))

(use-package whitespace
  :ensure t
  :defer t
  :custom-face
  (whitespace-space ((t (:background nil :foreground "lightgray")))))

(use-package org
  :ensure t
  :defer t
  :custom
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-clock-persist 'history)
  (org-src-fontify-natively t)
  (org-todo-keywords '((type "TODO" "TEST" "DONE")))
  (org-todo-keyword-faces '(("TODO" . (:foreground "#C38418" :weight bold))
                            ("TEST" . (:foreground "#C398D8" :weight bold))
                            ("DONE" . (:foreground "#335EA8" :weight bold))
                            ;; List settings.
                            ("PTW" . (:foreground "#C38418" :weight bold))
                            ("CUR" . (:foreground "#C398D8" :weight bold))
                            ("HLD" . (:foreground "#C398D8" :weight bold))
                            ("CMP" . (:foreground "#335EA8" :weight bold))
                            ("DRP" . (:foreground "#335EA8" :weight bold))))
  :config
  (org-clock-persistence-insinuate)
  ;; Add `C-c C-, r' shortcut for begin_src restclient block insertion.
  (add-to-list 'org-structure-template-alist '("r" . "src restclient
")))

(use-package ob-elixir
  :ensure t
  :quelpa
  (ob-elixir :fetcher git
             :branch "master"
             :url "https://git.sr.ht/~sokolov/ob-elixir"))

(use-package ob-restclient :ensure t)

(use-package babel
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((elixir . t)
     (restclient . t)
     (shell . t)
     (emacs-lisp . t)
     (scheme . t)
     (python . t)
     (sql . t)
     (haskell . t)
     (ocaml . t))))

(use-package geiser :ensure t)
(use-package geiser-guile :ensure t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package hide-mode-line :ensure t)

(use-package epresent
  :ensure t
  :custom (epresent-text-scale 200))

(use-package project
  :config
  (defun my/project-try-mix (dir)
    (when-let ((mix-dir (locate-dominating-file dir "mix.exs")))
      (cons 'mix mix-dir)))
  (cl-defmethod project-root ((project (head mix)))
      (cdr project))
  (add-to-list 'project-find-functions #'my/project-try-mix))

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "/usr/bin/pandoc")
  :custom-face
  (markdown-pre-face ((nil (:inherit org-block :font "Iosevka 12"))))
  :bind
  (:map markdown-mode-map
        ("C-c C-c v" . 'my/switch-markdown-mode)))

(use-package auto-fill-mode
  :custom (fill-collumn 80)
  :hook text-mode markdown-mode org-mode)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  :custom-face (sp-pair-overlay-face (nil ((:background "#EEE")))))

;; Note: Causes performance issues.
;; (use-package smooth-scroll
;;   :ensure t
;;   :config (smooth-scroll-mode 1))

(use-package sublime-themes
  :ensure t
  :config (load-theme 'mccarthy t))

(use-package sublimity
  :ensure t
  :bind ([f9] . 'sublimity-mode))

(use-package sublimity-map
  :defer t
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
  :defer t
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

(use-package tramp
  :ensure t
  :defer t
  :custom (tramp-default-method "ssh"))

(defun ssh-shell (host)
  (interactive "sHost: ")
  (let ((default-directory (concat "/ssh:" host ":"))
        (explicit-shell-file-name "sh"))
    (shell)))

(defun ssh-docker-shell (host container)
  (interactive "sHost: \nsContainer: ")
  (let ((default-directory (concat "/ssh:" host ":docker:" container ":"))
        (explicit-shell-file-name "sh"))
    (shell)))

(defun ssh-docker-psql (host container)
  (interactive "sHost: \nsContainer: ")
  (let ((default-directory (concat "/ssh:" host ":docker:" container ":")))
    (funcall-interactively #'sql-postgres)))

(defun ssh-psql (host container)
  (interactive "sHost: \nsContainer: ")
  (let ((default-directory (concat "/ssh:" host ":docker:" container ":"))
        (sql-postgres-program (concat "ssh -t " host
                                      " docker exec -it " container
                                      " psql")))
    (sql-postgres)))

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode +1)
  :custom
  (git-gutter:always-show-gutter t)
  (git-gutter:window-width 1)
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
  :defer t)

(use-package magit-todos
  :ensure t
  :defer t
  :after magit)

(use-package forge
  :ensure t
  :defer t
  :after magit
  :config
  (setq auth-sources '("~/.authinfo"))
  (add-to-list
   'forge-alist
   '("git.elonsoft.ru" "git.elonsoft.ru/api/v4" "git.elonsoft.ru" forge-gitlab-repository)))

(use-package ranger
  :ensure t
  :bind ("C-x C-d" . 'deer)
  :custom
  (ranger-cleanup-eagerly t)
  (ranger-show-hidden t)
  :custom-face
  (hl-line ((nil (:background "#FFF"))))
  :config
  (ranger-override-dired-mode t))

(defun my/make-cursor-a-bar ()
  (message "kek")
  (set-default cursor-type 'bar))
(advice-add 'ranger-open-file :after
              #'my/make-cursor-a-bar)

;; (defun ranger-open-file (&optional mode)
;; (setq-default cursor-type 'bar)

(use-package dired-ranger
  :ensure t
  :bind (:map ranger-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package auto-complete
  :ensure t
  :defer t
  :config (auto-complete-mode 1))

(use-package company
  :ensure t
  :config (company--idle-delay)
  :hook (after-init . global-company-mode)
  :bind ("<backtab>" . 'company-complete-common-or-cycle))

(use-package company-quickhelp
  :ensure t
  :defer t
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
  ("C-c C-<" . 'mc/mark-all-like-this)
  :custom
  (mc/always-run-for-all 1))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . 'swiper-isearch)
  ("C-M-s" . 'swiper-isearch-thing-at-point)
  :custom-face
  (swiper-line-face ((nil (:background "#ffffaa"))))
  (swiper-match-face-1 ((nil (:background "#ffffaa")))))

(defun counsel-maybe-projectile-switch-to-buffer ()
  (interactive)
  (if (projectile-project-p)
      (funcall-interactively #'counsel-projectile-switch-to-buffer)
    (funcall-interactively #'counsel-switch-buffer)))

(use-package counsel
  :ensure t
  :custom
  (projectile-project-switch-action 'counsel-projectile-switch-to-buffer)
  (projectile-project-search-path '("~/wrk/"))
  :bind
  ("C-r" . 'counsel-projectile-ag)
  ("C-x p" . 'counsel-projectile-switch-project)
  ("C-x C-b" . 'counsel-switch-buffer)
  ("C-x b" . 'counsel-maybe-projectile-switch-to-buffer)
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

;; (use-package alchemist
;;   :ensure t
;;   :hook (elixir-mode . alchemist-mode))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :init
  ;; (add-to-list 'exec-path "/usr/lib/elixir-ls/")
  (add-to-list 'exec-path "~/.local/lib/elixir-ls/")
  ) ; language_server.sh

(use-package lsp-elixir
  :ensure t
  :hook (elixir-mode . lsp))

(use-package exunit
  :ensure t
  :bind
  (:map exunit-compilation-mode-map
        ("t" . toggle-truncate-lines)))

;; TODO!: Fix.
;; (use-package prettify-elixir
;;   :load-path "~/.emacs.d/mlibs/"
;;   ;; :hook elixir-mode
;;   )

(use-package elixir-mode
  :ensure t
  :hook
  ;; (elixir-format . my/prog/elixir-format-hook)
  (elixir-mode . dumb-jump-mode)
  (elixir-mode . subword-mode)
  (elixir-format-hook
   . (lambda ()
       (if (projectile-project-p)
           (setq elixir-format-arguments
                 (list "--dot-formatter"
                       (concat (locate-dominating-file buffer-file-name ".formatter.exs")
                               ".formatter.exs")))
         (setq elixir-format-arguments nil))))
  ;; (elixir-mode . prettify-elixir-mode)
  (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format 1 t)))
  (elixir-mode . (lambda ()
                   (setq prettify-symbols-alist
                         '(("|>" . ?▶)
                           ("->" . ?→)
                           ("<-" . ?←)
                           ("=>" . ?⇒)
                           ("<=" . ?≤)
                           (">=" . ?≥)
                           ("&" . ?λ)
                           ("&1" . ?ɑ)
                           ("&2" . ?β)
                           ("&3" . ?γ)
                           ("&4" . ?δ)
                           ("&5" . ?ε)
                           ("&6" . ?ζ)
                           ("&7" . ?η)
                           ("&8" . ?θ)
                           ("&9" . ?ι)
                           ("&10" . ?κ)
                           ("==" . ?≡)
                           ("fn" . ?λ)
                           ("~>" . ?↝)))
                   (prettify-symbols-mode)))
  :custom
  (show-trailing-whitespace t)
  (compilation-error-regexp-alist
   (cons
    '("^warning: \\(.*\\)\n  \\(\\(.*\\):\\([0-9]+\\): \\(.*\\)\\)$" 3 4 nil 1 2)
    compilation-error-regexp-alist))
  :bind
  (:map elixir-mode-map
        ("C-c a f c v" . 'my/ex-find-controller-for-view)
        ("C-c a f v c" . 'my/ex-find-view-for-controller)
        ("C-c a f c s" . 'my/ex-find-controller-for-swagger)
        ("C-c a f s c" . 'my/ex-find-swagger-for-controller)
        ("C-c a d" . 'elixir-mode-open-docs-stable)
        ;;
        ("C-c t c" . 'exunit-verify)
        ("C-c t ." . 'exunit-verify-single)
        ("C-c t a" . 'exunit-verify-all)
        ("C-c t t" . 'exunit-toggle-trace)
        ("C-c c c" . 'ex-compile)
        ("C-c c l" . 'ex-credo)
        ("C-c c f" . 'ex-format)))

;; (use-package iex-mode :load-path "~/.emacs.d/mlibs/")

(use-package iex-mode
  :defer t
  :quelpa
  (iex-mode :fetcher git
            :branch "rework"
            :url "https://git.sr.ht/~sokolov/iex-mode"))

(use-package inf-elixir
  :quelpa
  (inf-elixir :repo "J3RN/inf-elixir" :fetcher github)
  :bind
  (:map elixir-mode-map
        ("C-c x i l" . inf-elixir-send-line)
        ("C-c x i r" . inf-elixir-send-region)))

(defun projectile-run-iex (command)
  (interactive
   (list (read-shell-command "iex> " "iex -S mix")))
  (setq-local iex-default-directory-function (list #'projectile-project-root))
  (funcall-interactively #'run-iex command))

;; (defvar lsp-elixir--config-options (make-hash-table))

;; (add-hook 'lsp-after-initialize-hook
;;           (lambda ()
;;             (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))

(use-package pug-mode
  :ensure t
  :defer t
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
  :custom
  (lisp-body-indent 2)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  :hook (emacs-lisp-mode . my/prog/emacs-lisp-mode-hook)
  :bind
  ("C-c e e" . 'eval-region)
  ("C-c e r" . 'eval-print-last-sexp))

(defun my-delete-trailing-whitespace-over-elisp-format-region
    (fn &optional start end)
  (let ((start (or start (region-beginning)))
        (end (or end (region-end)))
        (end-marker (make-marker)))
    (move-marker end-marker end)
    (funcall fn start end)
    (delete-trailing-whitespace start end-marker)))

(advice-add 'elisp-format-region
            :around
            #'my-delete-trailing-whitespace-over-elisp-format-region)

(use-package typescript-mode
  :ensure t
  :custom
  (typescript-indent-level 2)
  :bind
  (:map typescript-mode-map
        ("C-x C-e" . 'ts-send-last-sexp)
        ("C-M-x" . 'ts-send-last-sexp-and-go)
        ("C-c b" . 'ts-send-buffer)
        ("C-c C-b" . 'ts-send-buffer-and-go)
        ("C-c l" . 'ts-load-file-and-go)))

;; (use-package ts-comint
;;   :ensure t
;;   :hook (typescript-mode . run-ts))

(use-package purescript-mode
  :ensure t
  :hook (purescript-mode . purescript-indentation-mode))

(use-package cc-mode
  :ensure t
  :custom
  (c-default-style "k&r")
  (c-basic-offset 2)
  (indent-tabs-mode nil)
  :hook (c-mode-common-hook . c-toggle-auto-state))

(use-package jedi
  :ensure t
  :defer t
  :custom (jedi:complete-on-dot 1))

(use-package python-mode
  :ensure t
  :defer t
  ;; :hook (python-mode . jedi:ac-setup)
  )

(use-package ispell
  :ensure t
  :defer t
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_US"))

(use-package js
  :ensure t
  :defer t
  :custom (js-indent-level 2))

(use-package sh-script
  :ensure t
  :defer t
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  (dolist (ext '("\\.html" "\\.js" "\\.css"))
    (add-to-list 'auto-mode-alist `(,ext . web-mode))))

(use-package nxml-mode
  :defer t
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-child-indent 4)
  (nxml-attribute-indent 4))

(defun my/ex-reformat-list ()
  (interactive)
  (replace-string ", " ",\n")
  (replace-string ",\n" ", ")
  (sp-up-sexp)
  (sp-select-previous-thing)
  (indent-for-tab-command))
(put 'set-goal-column 'disabled nil)

(use-package systemd
  :ensure t
  :defer t)


(use-package restclient
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.http" . restclient-mode)))

;; TODO!: Make it copy a path relative to project root.
(defun my/cp-current-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(use-package request
  :ensure t
  :defer t)

(load-file "~/.emacs.d/elonsoft-is.el")

(defun nhentai/title (number)
  (interactive "sGallery ID: ")
  (let ((url (concat "https://nhentai.net/g/" number "/json")))
    (request url
        :type "GET"
        :parser 'json
        :success
        (cl-function
         (lambda (&key data &allow-other-keys)
           (let ((title (assoc-default 'title data)))
             (message "Title: %s" title))))
        :code-status
        '((404 . (lambda (&rest _) (message "Not found")))))))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  :quelpa
  (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
  :bind
  ("s-`" . eshell-toggle))

(defun ex-migration-run ()
  "Run migration in current file"
  (interactive)
  (let* ((file-name (buffer-name (current-buffer)))
         (timestamp (car (split-string file-name "_")))
         (cmd (concat "mix ecto.migrate --to " timestamp))
         (buffer (get-buffer-create "*ExMigration*")))
    (with-current-buffer buffer
      (setq default-directory (projectile-project-root))
      (async-shell-command cmd buffer)
      (setq buffer-read-only t)
      (pop-to-buffer buffer))))

(use-package remember
  :bind
  ("C-c r" . 'remember)
  ("C-c C-r" . 'remember-region))

(defun ex-format ()
  "Run mix format on the current projectile elixir project"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (exunit-do-compile "mix format")))

(defun ex-credo ()
  "Run mix credo -a on the current projectile elixir project"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (exunit-do-compile "mix credo -a")))

(defun ex-compile ()
  "Run mix credo -a on the current projectile elixir project"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (exunit-do-compile "mix compile --warnings-as-errors")))

(use-package smerge-mode
  :defer t
  :custom
  (smerge-command-prefix "\C-cm")
  :config
  ;; Somehow it doesn't work without reevaluating this code.
  (easy-mmode-defmap smerge-mode-map
    `((,smerge-command-prefix . ,smerge-basic-map))
    ""))

(use-package sql-indent
  :ensure t
  :defer t
  :custom (sql-indent-offset 2))

(defun rust-check ()
  "Compile using `cargo check'"
  (interactive)
  (compile "cargo check"))

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . subword-mode)
  :custom
  (rust-format-on-save t)
  :bind
  (:map rust-mode-map
        ("C-c c b" . 'rust-compile)
        ("C-c c c" . 'rust-check)
        ("C-c c t" . 'rust-test)
        ("C-c c r" . 'rust-run)
        ("C-c c l" . 'rust-run-clippy)))

(use-package info-rename-buffer
  :ensure t
  :defer t
  :config (info-rename-buffer-mode))

(use-package beam-mode
  :defer t
  :load-path "~/.emacs.d/mlibs")

;; (use-package beam-file-mode
;;   :ensure t
;;   :quelpa
;;   (beam-file-mode :fetcher github :repo "legoscia/erlang-extra-modes"))

;;https://github.com/legoscia/erlang-extra-modes

(use-package json-snatcher
  :ensure t
  :defer t)

(load "~/.opam/default/share/emacs/site-lisp/tuareg-site-file")

(use-package go-mode :ensure t)

(use-package sml-mode
  :ensure t
  :defer t
  :custom (sml-font-lock-symbols t))

(defun shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

(use-package dockerfile-mode
  :ensure t
  :custom (dockerfile-use-sudo t))

(defun docker-compose-build-buffer (args image-name)
  (interactive "sArgs: \nsImage name: ")
  (save-buffer)
  (compilation-start
   (format
    "sudo docker build %s %s -f %s ."
    args
    (if (string= image-name "") "" (concat "-t " image-name))
    (shell-quote-argument (buffer-file-name)))
   nil
   (lambda (_) (format "*docker-compose-build-output: %s *" image-name))))

;; (use-package elisp-format
;;   :ensure t
;;   :custom
;;   (elisp-format-column 80)
;;   ;; (elisp-format-newline-keyword-except-list '("thread-last"))
;;   ;; (elisp-format-newline-keyword-addons-list '("object"))
;;   )

(use-package pp
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :custom (graphviz-dot-indent-width 2))

;; (use-package company-graphviz-dot :ensure t)

(defun sql-format-region (beg end)
  (interactive "r")
  (let ((command "python -c \
           'import sys, sqlparse; \
            print(sqlparse.format(sys.stdin.read(), reindent=True)) \
           '"))
    (shell-command-on-region beg end command t t)))

(use-package po-mode
  :defer t
  :load-path "~/.local/share/emacs/site-lisp/"
  :commands (po-mode))

;; From flashbacks.
(use-package elixir-compile
  :ensure t
  :defer t)

(use-package js-mode
  :hook (js-mode . subword-mode))

(use-package vue-mode
  :ensure t
  :custom
  (js-indent-level 2)
  (css-indent-offset 2)
  :hook
  (vue-mode . subword-mode)
  (vue-mode .
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil))))

;; (use-package pkgbuild-mode
;;   :quelpa
;;   (pkgbuild-mode :fetcher git
;;                  :branch "master"
;;                  :url "https://gitlab.com/stefanhusmann/pkgbuild-mode.git"))
