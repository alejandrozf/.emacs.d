;;; package --- Summary

;;; Commentary:

;;; Code:

(setq straight-use-package-by-defaul t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defvar my:backup-directory
  (expand-file-name (concat user-emacs-directory "backups/"))
  "Directory storing all backups and auto-save files.
Must end with a trailing slash.")

(setq inhibit-startup-message t)
(desktop-save-mode 1)
(global-linum-mode 1)
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)

(server-start) ;;or using "emacs --daemon option"

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-12"))

(load "~/.emacs.d/alezf")
(load "~/.emacs.d/desktop-menu")
(load "~/.emacs.d/bufsearch")
(load "~/.emacs.d/mail")
(load "~/.emacs.d/docker-tramp-compat")

(use-package ag
             :straight t
             :if (not noninteractive))

(use-package auto-complete
  :if (not noninteractive)
  :diminish auto-complete-mode
  :config (progn
            (require 'auto-complete-config)
            (ac-config-default)
            (setq-default ac-sources '(ac-source-yasnippet
                                       ac-source-filename
                                       ac-source-abbrev
                                       ac-source-dictionary
                                       ac-source-words-in-same-mode-buffers))
            (global-auto-complete-mode 1)))

(use-package cyberpunk-theme
  :straight t)

(use-package files
  :config
  (progn
    (setq auto-save-file-name-transforms `((".*" ,my:backup-directory t))
          auto-save-list-file-prefix my:backup-directory
          backup-directory-alist `(("." . ,my:backup-directory))
          auto-save-default t
          auto-save-interval 200
          auto-save-timeout 20
          backup-by-copying t
          delete-by-moving-to-trash t
          delete-old-versions t
          kept-new-versions 20
          kept-old-versions 0
          make-backup-files t
          version-control t)
    (defun force-backup-of-buffer ()
      "Reset `buffer-backed-up' to nil."
      (setq buffer-backed-up nil))
    ;; Always create backups on save:
    (add-hook 'before-save-hook #'delete-trailing-whitespace)
    (add-hook 'before-save-hook #'force-backup-of-buffer)))


(use-package flycheck
  :config
  (progn
    ;; Add virtualenv support for checkers
    (defadvice flycheck-checker-executable
        (around python-flycheck-check-executable (checker)
                activate compile)
      "`flycheck-checker-executable' with virtualenv support."
      (if (eq major-mode 'python-mode)
          (let* ((process-environment (python-shell-calculate-process-environment))
                 (exec-path (python-shell-calculate-exec-path)))
            ad-do-it)
        ad-do-it))

    (defadvice flycheck-start-checker
        (around python-flycheck-start-checker (checker callback)
                activate compile)
      "`flycheck-start-checker' with virtualenv support."
      (if (eq major-mode 'python-mode)
          (let* ((process-environment (python-shell-calculate-process-environment))
                 (exec-path (python-shell-calculate-exec-path)))
            ad-do-it)
        ad-do-it))

    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))

    (setq flycheck-mode-line-lighter " ")))

;; this is for the autocomplete for commands
(use-package ido
  :if (not noninteractive)
  :config
  (progn
    (use-package ido-vertical-mode)
    (use-package flx)
    (use-package flx-ido)
    (setq ido-enable-flex-matching t
          ido-use-faces nil
          flx-ido-use-faces t)
    (ido-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)
    (flx-ido-mode 1)))

(use-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

    (setq web-mode-engines-alist
          '(("blade"  . "\\.blade\\.")
            ("django" . "\\.html")))

    (defun my-web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 4)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-comment-style 2)
      (setq web-mode-enable-current-element-highlight t)
      (set-face-background 'web-mode-current-element-highlight-face "#0000FF")
      (local-set-key (kbd "C-c C-v") 'browse-url-of-buffer))
    (add-hook 'web-mode-hook  'my-web-mode-hook)))

;; Setting 2 spaces on javascript-mode
(setq js-indent-level 4)

(use-package projectile
  :if (not noninteractive)
  :diminish projectile-mode
  :config (projectile-global-mode 1))

(use-package python
  :config
  (progn
    (use-package jedi)
    (setq jedi:complete-on-dot t)
    (remove-hook 'python-mode-hook 'wisent-python-default-setup)
    (add-hook 'python-mode-hook 'jedi:setup)
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'hs-minor-mode)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i" )))

(use-package magit
  :if (not noninteractive)
  :bind (("C-x g" . magit-status)
         ("C-x p" . magit-push))
  :config
  (progn
    (defun magit-diff-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-diff-dont-ignore-whitespace)
        (magit-diff-ignore-whitespace)))
    (defun magit-diff-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))
    (defun magit-diff-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))
    (defun magit-merge-no-ff (rev)
      (interactive (list (magit-read-other-branch-or-commit "Merge")))
                   (magit-merge-assert)
                   (magit-run-git "merge"  "--no-ff" rev))
    (bind-key "W" 'magit-diff-toggle-whitespace magit-status-mode-map)))

;; colour for your parens...
(use-package rainbow-mode
  :if (not noninteractive)
  :config (progn
            (mapc (lambda (mode)
                    (add-to-list 'rainbow-r-colors-major-mode-list mode))
                  '(css-mode emacs-lisp-mode lisp-interaction-mode))
            (add-hook 'prog-mode-hook #'rainbow-turn-on)))

(use-package rainbow-delimiters
  :if (not noninteractive)
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package scroll-bar
  :config (scroll-bar-mode -1))

(use-package smartparens
  :if (not noninteractive)
  :diminish (smartparens-mode . " π"))

(use-package smex
  :if (not noninteractive)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package uniquify
  :if (not noninteractive)
  :config (setq uniquify-buffer-name-style 'forward))

(use-package whitespace
  :if (not noninteractive)
  :diminish (global-whitespace-mode . " ω")
  :config (progn
            (setq whitespace-style '(trailing tabs indentation::space face))
            (setq whitespace-global-modes
                  '(c-mode c++-mode emacs-lisp-mode python-mode lisp-mode go-mode))
            (global-whitespace-mode 1)))


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(put 'dired-find-alternate-file 'disabled nil)


(global-flycheck-mode)


;; Python virtualenv support configuration
(use-package virtualenvwrapper
  :straight t
  :config (progn (venv-initialize-interactive-shells) ;; if you want interactive shell support
                 (venv-initialize-eshell) ;; if you want eshell support
                 (setq venv-location "~/.virtualenvs/")))

;; Configuring emmet-mode for (x)html & css files
(use-package emmet-mode
    :straight t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

(use-package bash-completion
  :straight t
  :config (bash-completion-setup))

(use-package restclient
    :straight t)

(use-package yaml-mode
    :straight t)

(use-package ansible
  :straight t
  :config
  ;; activamos yaml-mode cuando se activa ansible-mode
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

(use-package yasnippet
  :straight t
  :config
  (progn
    (add-to-list 'load-path
                 "~/.emacs.d/plugins/yasnippet")
    (yas-global-mode 1)))

;; para cargar los snippets usar https://github.com/AndreaCrotti/yasnippet-snippets:
;; M-x package-refresh-contents
;; M-x package-install yasnippet-snippets

(use-package docker
  :bind ("C-c d" . docker))

(use-package slime-docker
    :straight t)

(load "~/.emacs.d/asdf")
;; (slime-setup '(slime-fancy slime-tramp))
(setq inferior-lisp-program "ros run")
;; (setq inferior-lisp-program (expand-file-name "~/ccl/./lx86cl64"))
;; (setq inferior-lisp-program (concat "java -jar " (expand-file-name "~/abcl/abcl.jar")))
(setq slime-contribs '(slime-fancy))

;; define function to restart the server
(defun signal-restart-server ()
  (interactive)
  (message "Caught event %S" last-input-event)
  (server-mode)
  )

;; add the binding to the special-event-map
(define-key special-event-map [sigusr1] 'signal-restart-server)

;; cua-mode initialization
(cua-mode t)

;; delete beep, and change by visible indicator
(setq visible-bell 1)

;; ver la hora por defecto
(display-time-mode 1)

(condition-case nil
    (load "~/.emacs.d/local")
  (error "No ha definido un archivo con código local"))

;; Org-mode clock tasks
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq dired-dwim-target t)

(org-babel-do-load-languages
   'org-babel-load-languages
   '((calc . t)
     (python . t)
     (lisp . t)
     (shell . t)
     ))

(use-package dumb-jump
  ;; instalar ag
  ;; https://github.com/ggreer/the_silver_searcher
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ido) ;; (setq dumb-jump-selector 'helm)
)

(add-hook 'after-init-hook
          () (load-theme 'cyberpunk t))

(add-hook 'after-org-mode
          () (setq org-indent-mode t))


(provide 'init)

;;; init.el ends here
