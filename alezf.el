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


(fset 'ipdb
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([105 112 backspace 109 112 111 114 116 32 105 112 115 backspace 100 98 59 105 112 100 98 backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace 112 111 114 116 32 105 112 100 98 59 105 112 100 98 46 115 101 116 95 116 114 97 99 101 40 41] 0 "%d")) arg)))

(fset 'comment-out-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 5 134217787] 0 "%d")) arg)))

(fset 'dup-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 5 3 timeout return 22] 0 "%d")) arg)))

(fset 'del-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("" 0 "%d")) arg)))

(fset 'template_block
   [?\{ ?% ?  ?  left])

(fset 'template_var
   [?\{ ?\{ ?  ?  left])

(fset 'copy-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 5 3 timeout] 0 "%d")) arg)))

(fset 'clojure-enviroment
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([C-f9 134217848 99 108 111 106 117 114 101 return 134217848 99 105 100 101 114 109 111 100 101 return 3 134217834 121] 0 "%d")) arg)))

(fset 'set_virtualenv_python_mode
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 118 101 110 118 return 111 109 105 tab return 134217848 112 121 116 104 111 110 return] 0 "%d")) arg)))

;; wrapper around slime-docker, for its correct use first:
;; 1) $ mkdir ~/projects (if not exists folder)
;; 2) $ docker network create devnetwork
(fset 'my-docker
      (lambda (&optional arg) (interactive)
        (slime-docker-start :image-name "daewok/lisp-devel" :image-tag "latest" :rm t
                            :mounts '((("~/projects" . "/home/lisp/quicklisp/local-projects/")))
                            :network "devnetwork" :env '(("LISP_DEVEL_UID" . "1000")))))


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

(global-set-key (kbd "<f5> l") 'copy-line)

(global-set-key (kbd "<f5> i") 'ipdb)

(global-set-key (kbd "<C-f5>") 'revert-buffer)

(global-set-key (kbd "<f5> c") 'comment-out-line)

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

(provide 'alezf)
;;; alezf.el ends here
