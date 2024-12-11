;;; -*- lexical-binding: t; -*-
;;; Local Variables:
;;; eval: (add-hook 'after-save-hook #'byte-compile-current-file nil t)
;;; End:
;;; Emacs config.

;; (setq debug-on-error t)

(defun my/set-system-font (font)
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font)
  (set-face-attribute 'default nil :font font)
  (set-frame-font font nil t))

(defun my/comment-or-uncomment ()
  (interactive)
  (if (region-active-p)
    (funcall-interactively 'comment-or-uncomment-region
                           (region-beginning)
                           (region-end))
    (funcall-interactively 'comment-line 1)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)
(text-scale-adjust +1)
(my/set-system-font "Iosevka Light-12")
(mapcar (lambda (x) (put x 'disabled nil))
        '(narrow-to-page
          narrow-to-region
          scroll-left
          downcase-region
          upcase-region))
(defalias 'yes-or-no-p 'y-or-n-p)
;; (add-to-list 'load-path "~/.emacs.d/mlibs/")

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups" t)))
(setq create-lockfiles nil)
(setq-default cursor-type 'bar)

(let ((content
       '(let* ((project (project-current))
               (project-content
                (when project
                  (concat "[" (project-name project) "]")))
               (current (buffer-file-name))
               (buffer-content
                (when current
                  (if project
                      (concat "/" (file-relative-name current (project-root project)))
                    current))))
          (concat project-content buffer-content))))
  (setq frame-title-format `("Emacs " (:eval ,content))))

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(require 'sublime-themes)
(load-theme 'mccarthy t)

(require 'mood-line)
(mood-line-mode)
(set-face-attribute 'mode-line nil :background "#335EA8")
(set-face-attribute 'mode-line-inactive nil :background "#9B9C97")
;; File position.
(set-face-attribute 'mood-line-unimportant nil :foreground "#EEE")
;; Not modified git line
;; (set-face-attribute 'mood-line-status-grayed-out nil :foreground "#EEE")
;; Modified git line.
(set-face-attribute 'mood-line-status-info nil :foreground "#EEE")

(with-eval-after-load 'hl-line
  ;; (require 'hl-line)
  (set-face-attribute 'hl-line nil
                      :background "#335EA8"
                      :foreground "#EEE"
                      :underline nil))

(require 'reverse-im)
(reverse-im-activate "russian-computer")

(require 'multiple-cursors)
(bind-key (kbd "C-.") 'mc/mark-next-like-this)
(bind-key (kbd "C->") 'mc/unmark-next-like-this)
(bind-key (kbd "C-,") 'mc/mark-previous-like-this)
(bind-key (kbd "C-<") 'mc/unmark-previous-like-this)
(bind-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; rainbow-delimiters
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; bookmarks
(require 'bookmark)
(setq bookmark-save-flag 1)
(bind-key (kbd "C-x r k") 'bookmark-delete)

(with-eval-after-load "org"
  (setq org-adapt-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-clock-persist 'history)
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords '((type "TODO" "TEST" "DONE")))
  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "#C38418" :weight bold))
	  ("TEST" . (:foreground "#C398D8" :weight bold))
	  ("DONE" . (:foreground "#335EA8" :weight bold))
	  ;; List settings.
	  ("PTW" . (:foreground "#C38418" :weight bold))
	  ("CUR" . (:foreground "#C398D8" :weight bold))
	  ("HLD" . (:foreground "#C398D8" :weight bold))
	  ("CMP" . (:foreground "#335EA8" :weight bold))
	  ("DRP" . (:foreground "#335EA8" :weight bold))))
  ;;
  (set-face-attribute 'org-block nil :font "Iosevka 12")
  (set-face-attribute 'org-meta-line nil :font "Iosevka 12")
  (set-face-attribute 'org-table nil :font "Iosevka 12")
  (org-clock-persistence-insinuate)
  ;; Add `C-c C-, r' shortcut for begin_src restclient block insertion.
  (add-to-list 'org-structure-template-alist '("r" . "src restclient
"))
  ;;
  (add-hook 'org-mode-hook 'org-bullets-mode))

(require 'epresent)
(setq epresent-text-scale 200)

;; (require 'auto-fill-mode)
;; (setq fill-collumn 80)

(global-set-key (kbd "C-;") #'my/comment-or-uncomment)
(global-set-key (kbd "C-x k") #'kill-current-buffer)
(global-set-key (kbd "C-x n f") #'fold-this)
(global-set-key (kbd "C-x n u") #'fold-this-unfold-at-point)

(global-set-key (kbd "C-c i")
		(lambda ()
		  (interactive)
		  (require 'string-inflection)
		  (string-inflection-cycle)))

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
   (ocaml . t)
   (js . t)
   (plantuml . t)))

(defun my/project-try-mix (dir)
  (when-let ((mix-dir (locate-dominating-file dir "mix.exs")))
    (cons 'mix mix-dir)))
(cl-defmethod project-root ((project (head mix)))
  (cdr project))
(add-to-list 'project-find-functions #'my/project-try-mix)

(with-eval-after-load 'text-mode
  ;; (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'text-mode-hook 'visual-line-mode))

(with-eval-after-load 'markdown-mode
  ;; (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(with-eval-after-load 'org-mode
  ;; (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'visual-line-mode))

(bind-key (kbd "M-o")
	  (lambda ()
	    (interactive)
	    (other-window 1)))

;; ranger
(bind-key (kbd "C-x C-d") 'deer)
(setq ranger-cleanup-eagerly t)
(setq ranger-show-hidden t)
(ranger-override-dired-mode t)

;; Do we need it for tramp?
(require 'cl-lib)

(autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
;; (setq tramp-archive-file-name-regexp nil)
;; (setq tramp-completion-function-alist-rsh nil)
(require 'tramp)
(setq tramp-default-method "ssh")

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq dumb-jump-selector 'ivy)

(require 'which-key)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 1)
(setq which-key-idle-secondary-delay 0.05)
(mapcar (lambda (key) (add-to-list 'which-key-replacement-alist key))
        '((("TAB" . nil) . ("↹" . nil))
          (("RET" . nil) . ("⏎" . nil))
          (("DEL" . nil) . ("⇤" . nil))
          (("SPC" . nil) . ("␣" . nil))))
(which-key-mode)

(require 'swiper)
(bind-key (kbd "C-s") #'swiper)
(bind-key (kbd "C-M-s") #'swiper-isearch-thing-at-point)
(set-face-attribute 'swiper-line-face nil :background "#ffffaa")
(set-face-attribute 'swiper-match-face-1 nil :background "#ffffaa")

(require 'counsel)

(defun counsel-maybe-project-switch-to-buffer ()
  (interactive)
  (if (eq nil (project-current))
      (funcall-interactively #'counsel-switch-buffer)
    (funcall-interactively #'project-switch-to-buffer)))

(bind-key (kbd "C-r") #'counsel-ag)
(bind-key (kbd "C-x p") #'project-switch-project)
(bind-key (kbd "C-x C-b") #'counsel-switch-buffer)
;; (bind-key (kbd "C-x b") #'counsel-maybe-project-switch-to-buffer)
(bind-key (kbd "C-x b") #'project-switch-to-buffer)
(bind-key (kbd "C-x f") #'project-find-file)
(bind-key (kbd "C-x i") #'counsel-imenu)

;; smartparens-mode
(require 'smartparens-config)
(smartparens-global-mode +1)
(set-face-attribute 'sp-pair-overlay-face nil :background "#EEE")

;; paren
(show-paren-mode 1)

;; pug-mode
(with-eval-after-load "pug-mode"
  (setq pug-tab-width 2))

;; elisp-mode
(with-eval-after-load "elisp-mode"
  (setq lisp-body-indent 2)
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil)
  ;;
  (bind-key (kbd "C-c e e") #'eval-region)
  (bind-key (kbd "C-c e r") #'eval-print-last-sexp))

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

;; typescript-mode
(with-eval-after-load "typescript-mode"
  (setq typescript-indent-level 2))

;; ispell
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")

;; web-mode
(with-eval-after-load "web-mode"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(dolist (ext '("\\.html" "\\.js" "\\.css" "\\.html.eex" "\\.pdf.eex" "\\.tsx"))
  (add-to-list 'auto-mode-alist `(,ext . web-mode)))

(add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))

(defun my/project-file-name ()
  (require 'project)
  (let ((project (project-current))
        (current (buffer-file-name)))
    (when (and project current)
      (file-relative-name current (project-root project)))))

(defun my/cp-project-file-name ()
  (interactive)
  (let ((filename (my/project-file-name)))
    (when filename
      (kill-new filename)
      (message "Copied project file name '%s' to the clipboard." filename))))

(defun my/cp-current-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; eshell-toggle
(setq eshell-toggle-size-fraction 3)
(setq eshell-toggle-use-projectile-root t)
(setq eshell-toggle-run-command nil)
(setq eshell-toggle-init-function #'eshell-toggle-init-ansi-term)

;; git-gutter
(global-git-gutter-mode +1)
(setq git-gutter:update-interval 2)
;; (set git-gutter:ask-p nil "Don't ask on revert.")
(setq git-gutter:modified-sign " ")
(setq git-gutter:added-sign " ")
(setq git-gutter:deleted-sign " ")

(set-face-attribute 'git-gutter:modified nil :background "#C38418")
(set-face-attribute 'git-gutter:added nil :background "#335EA8")
(set-face-attribute 'git-gutter:deleted nil :background "#F22C40")

(bind-key (kbd "C-c g n") 'git-gutter:next-hunk)
(bind-key (kbd "C-c g p") 'git-gutter:previous-hunk)
(bind-key (kbd "C-c g r") 'git-gutter:revert-hunk)
(bind-key (kbd "C-c g c") 'git-gutter:stage-hunk)
(bind-key (kbd "C-c g s") 'git-gutter:popup-hunk)
(bind-key (kbd "C-c g m") 'git-gutter:mark-hunk)

;; git-timemachine
(bind-key (kbd "C-c g t") 'git-timemachine)

;; gitignore
(with-eval-after-load 'vc-hooks
  (add-to-list 'vc-directory-exclusion-list ".direnv")
  (add-to-list 'vc-directory-exclusion-list ".envrc")
  (add-to-list 'vc-directory-exclusion-list ".elixir_ls")
  (add-to-list 'vc-directory-exclusion-list ".lexical")
  (add-to-list 'vc-directory-exclusion-list "./_build")
  (add-to-list 'vc-directory-exclusion-list "./deps")
  (add-to-list 'vc-directory-exclusion-list "./cover"))

;; auto-complete
(auto-complete-mode 1)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(bind-key (kbd "<backtab>") 'company-complete-common-or-cycle)

;; remember
(require 'remember)
(bind-key (kbd "C-c r") 'remember)
(bind-key (kbd "C-c C-r") 'remember-region)

;; smerge-mode
(setq smerge-command-prefix "\C-cm")
(with-eval-after-load "smerge-mode"
  (easy-mmode-defmap smerge-mode-map
    `((,smerge-command-prefix . ,smerge-basic-map))
    ""))

(require 'sql-indent)
(setq sql-indent-offset 2)

(with-eval-after-load "rust-mode"
  (setq rust-format-on-save t)
  (setq rust-rustfmt-switches '("--edition" "2018"))
  ;;
  (add-hook 'rust-mode-hook 'eglot)
  (add-hook 'rust-mode-hook 'subword-mode))
;; (:map rust-mode-map
;;       ("C-c c b" . 'rust-compile)
;;       ("C-c c c" . 'rust-check)
;;       ("C-c c t" . 'rust-test)
;;       ("C-c c r" . 'rust-run)
;;       ("C-c c l" . 'rust-run-clippy))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((elixir-mode elixir-ts-mode) "lexical")))

(with-eval-after-load 'elixir-ts-mode
  (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
  (add-hook 'elixir-ts-mode-hook 'dumb-jump-mode)
  (add-hook 'elixir-ts-mode-hook 'subword-mode))

(require 'info-rename-buffer)
(info-rename-buffer-mode)

(use-package beam-mode :load-path "~/.emacs.d/mlibs")

;; dockerfile-mode
(setq dockerfile-use-sudo t)

;; graphviz-dot-mode
(setq graphviz-dot-indent-width 2)

;; flycheck
(setq ispell-dictionary "ru_RU")

;; plantuml-mode
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(setq plantuml-default-exec-mode 'executable)
(setq org-plantuml-exec-mode 'plantuml)
(setq plantuml-executable-path "/usr/bin/plantuml")
(setq org-plantuml-executable-path "/usr/bin/plantuml")

;; direnv
(envrc-global-mode)

;; helpers

(defun sum-check (list)
  "Convinience function that sums check entries.

Basically it just sums second items in cons pairs in the list. For
example, given the following code

    (sum-check `((item-1 . 10)
                 (item-2 . 20)))

it will give you `30'.
"
  (thread-last list
    (seq-map #'cdr)
    ((lambda (list)
       (seq-reduce #'+ list 0)))))

;; Dice rolling.

(defun dice (arg)
  (interactive "P")
  (funcall (if arg #'insert
             (apply-partially #'message "%c"))
           (aref "⚀⚁⚂⚃⚄⚅" (random 6))))
