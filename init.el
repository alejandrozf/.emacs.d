(defvar package-archives)

(defvar package-archive-contents)

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
(load "~/.emacs.d/alezf")
(load "~/.emacs.d/bufsearch")

(setq package-archives '(("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; this installs use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package ag
  :if (not noninteractive)
  :ensure ag)

(use-package auto-complete
  :if (not noninteractive)
  :ensure auto-complete
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
  :ensure cyberpunk-theme)

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
  :ensure flycheck
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
    (use-package ido-vertical-mode
      :ensure ido-vertical-mode)
    (use-package flx
      :ensure flx)
    (use-package flx-ido
      :ensure flx-ido)
    (setq ido-enable-flex-matching t
          ido-use-faces nil
          flx-ido-use-faces t)
    (ido-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)
    (flx-ido-mode 1)))

(use-package web-mode
  :ensure web-mode
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
(setq js-indent-level 2)

;; (use-package powerline
;;   :if (not noninteractive)
;;   :ensure powerline
;;   :config (powerline-default-theme))

(use-package projectile
  :if (not noninteractive)
  :diminish projectile-mode
  :ensure projectile
  :config (projectile-global-mode 1))

(use-package python
  :config
  (progn
    (use-package jedi
      :ensure jedi)
    (setq jedi:complete-on-dot t)
    (remove-hook 'python-mode-hook 'wisent-python-default-setup)
    (add-hook 'python-mode-hook 'jedi:setup)
    (add-hook 'python-mode-hook 'flycheck-mode)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i" )))

(use-package python-django
  :if (not noninteractive)
  :bind ("C-x j" . python-django-open-project)
  :ensure python-django)

(use-package magit
  :if (not noninteractive)
  :bind (("C-x g" . magit-status)
         ("C-x p" . magit-push))
  :ensure magit
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
  :ensure rainbow-mode
  :config (progn
            (mapc (lambda (mode)
                    (add-to-list 'rainbow-r-colors-major-mode-list mode))
                  '(css-mode emacs-lisp-mode lisp-interaction-mode))
            (add-hook 'prog-mode-hook #'rainbow-turn-on)))

(use-package rainbow-delimiters
  :if (not noninteractive)
  :ensure rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package scroll-bar
  :config (scroll-bar-mode -1))

(use-package dumb-jump
  :ensure dumb-jump
  :config (dumb-jump-mode))

(use-package smartparens
  :if (not noninteractive)
  :ensure smartparens
  :diminish (smartparens-mode . " π"))

(use-package smex
  :if (not noninteractive)
  :ensure smex
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
  :ensure virtualenvwrapper
  :config (progn (venv-initialize-interactive-shells) ;; if you want interactive shell support
                 (venv-initialize-eshell) ;; if you want eshell support
                 (setq venv-location "~/.virtualenvs/")))
;; (venv-workon "ceg")

;; Configuring emmet-mode for (x)html & css files
(use-package emmet-mode
  :ensure emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

;; Beauty and functional text explorer
(use-package sr-speedbar
  :ensure sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)

;;Ein (Emacs-Ipython config)
(use-package ein
  :ensure ein)

(use-package bash-completion
  :ensure bash-completion
  :config (bash-completion-setup))

(use-package restclient
  :ensure restclient)

(use-package yaml-mode
  :ensure yaml-mode)

(use-package ansible-mode
  :ensure ansible-mode
  :config
  ;; activamos yaml-mode cuando se activa ansible-mode
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

;; (use-package yasnippet
;;   :ensure yasnippet
;;   :config
;;   (progn
;;     (add-to-list 'load-path
;;                  "~/.emacs.d/plugins/yasnippet")
;;     (yas-global-mode 1)))

;;Slime config
(condition-case nil
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (error "Yo may want to install quicklisp & slime-helper"))

(load "~/.emacs.d/asdf")
(setq inferior-lisp-program "sbcl")
;; (setq inferior-lisp-program (expand-file-name "~/ccl/./lx86cl64"))
;; (setq inferior-lisp-program (concat "java -jar " (expand-file-name "~/abcl/abcl.jar")))
(setq slime-contribs '(slime-fancy))

(condition-case nil
    (use-package ox-reveal
      :ensure  ox-reveal)
  (error "Pending for review why ox-reveal give problems on automatic install"))

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

(provide 'init)

;;; init.el ends here
