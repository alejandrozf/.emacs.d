;;; alezf.el --- Summary

;;; Commentary:

;;; Code:

(fset 'magit-log-current-day
      (lambda (&optional arg) (interactive "p")
        "Returns list of git commit on last day"
        (setq buffer-name (format "magit-%d" (random 100)))
        (with-output-to-temp-buffer buffer-name
          (setq today (format-time-string "%Y-%m-%d"))
          (setq result (shell-command-to-string (format "git log --after='%s 00:00' --before='%s 23:59'" today today)))
          (switch-to-buffer buffer-name)
          (insert result))))


(defun kill-process-port ()
  "Kill process running on some port"
  (interactive)
  (let* ((port (string-to-number (read-string "Port:")))
         (str-command (format "fuser -k -n tcp %d" port)))
    (async-shell-command str-command)))

(defun restart-desktop ()
  "Restart desktop blocked by .lock file.
  It remove .lock file and load desktop"
  (interactive)
  (if (file-exists-p "~/.emacs.d/.emacs.desktop.lock")
      (delete-file "~/.emacs.d/.emacs.desktop.lock"))
  (desktop-read)
  (load-file "~/.emacs.d/init.el"))

(defun racket/put-breakpoint ()
  (interactive)
  (let* ((curr-pos (point))
         (dbg-str "(require debug/repl)")
         (dgb-str-len (length dbg-str))
         (breakpoint "(debug-repl) "))
    ;; Assuming first line have #lang directive
    (insert breakpoint)
    (goto-line 2)
    (insert dbg-str)
    (newline)
    (goto-char (+ dgb-str-len curr-pos))))

(fset 'template_block
   [?\{ ?% ?  ?  left])

(fset 'template_var
   [?\{ ?\{ ?  ?  left])

(fset 'turn-off-whitespace-mode (lambda () (interactive) (whitespace-mode 0)))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-08-11"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (auto-complete-mode 1)
    (setq buffer-offer-save t)))


(defun perl-on-buffer ()
  "Run perl with the contents in a buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "perl" "*Perl Output*")
  (display-buffer "*Perl Output*"))

(eval-after-load 'perl-mode
  '(define-key perl-mode-map (kbd "C-M-x") 'perl-on-buffer))

(defun dup-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )

(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
  (message "1 line copied"))

(defun del-line ()
  (interactive)
  (move-beginning-of-line 1)
  (delete-region (point) (line-end-position)))


(global-set-key (kbd "<f5> l") 'copy-line)

(global-set-key (kbd "<f5> i") 'ipdb)

(global-set-key (kbd "<C-f5>") 'revert-buffer)

(global-set-key (kbd "<f5> c") 'comment-line)

(global-set-key (kbd "<f5> d") 'dup-line)

(global-set-key (kbd "<f5> k") 'del-line)

(global-set-key (kbd "<f5> t") 'template_block)

(global-set-key (kbd "<f5> v") 'template_var)

(global-set-key [f6] 'shell)

(global-set-key [f8] 'dired-other-window)

(global-set-key (kbd "<C-tab>") 'other-window)

(global-set-key (kbd "<f5> r") 'magit-log-current-day)

(global-set-key (kbd "<f5> <f5>") 'restart-desktop)

(global-set-key (kbd "<C-f1>") 'delete-other-windows)

(global-set-key (kbd "<C-f2>") 'split-window-below)

(global-set-key (kbd "<C-f3>") 'split-window-right)

(global-set-key (kbd "<C-escape>") 'delete-window)

(global-set-key (kbd "<C-f9>") 'xah-new-empty-buffer)

(global-set-key (kbd "<f5> x") 'kill-emacs)

(global-set-key (kbd "<f5> j") 'clojure-enviroment)

(global-set-key (kbd "<f5> y") 'set_virtualenv_python_mode)

(global-set-key (kbd "<f5> f") 'find-file-at-point)

(global-set-key (kbd "C-c f") 'projectile-find-file)

(global-set-key (kbd "M-s") 'backward-kill-word)

(global-set-key (kbd "<f5> s") 'my-docker)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-;") 'sly-mrepl)

(global-set-key (kbd "<f5> w") 'turn-off-whitespace-mode)

(global-set-key (kbd "<f5> e") (lambda () (interactive)
                                 (erc :server "irc.libera.chat" :port "6667"
                                      :nick "alejandrozf")))
(global-set-key [f12] 'sly-mrepl)

(defun sly-start-all ()
  (interactive)
  (dolist (lisp '("sbcl" "ecl" "ccl" "clisp" "abcl"))
    (ignore-errors (sly-start :program lisp))
    (sleep-for 7)))

(provide 'alezf)
;;; alezf.el ends here
