(fset 'magit-log-current-day
      (lambda (&optional arg) (interactive "p")
        "Returns list of git commit on last day"
        (setq buffer-name (format "magit-%d" (random 100)))
        (with-output-to-temp-buffer buffer-name
          (setq today (format-time-string "%Y-%m-%d"))
          (setq result (shell-command-to-string (format "git log --after='%s 00:00' --before='%s 23:59'" today today)))
          (switch-to-buffer buffer-name)
          (insert result))))

(defun restart-desktop ()
  "Restart desktop blocked by .lock file.
  It remove .lock file and load desktop"
  (interactive)
  (if (file-exists-p "~/.emacs.d/.emacs.desktop.lock")
      (delete-file "~/.emacs.d/.emacs.desktop.lock"))
  (desktop-read)
  (load-file "~/.emacs.d/init.el"))

(fset 'ipdb
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([105 112 backspace 109 112 111 114 116 32 105 112 115 backspace 100 98 59 105 112 100 98 backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace backspace 112 111 114 116 32 105 112 100 98 59 105 112 100 98 46 115 101 116 95 116 114 97 99 101 40 41] 0 "%d")) arg)))

(fset 'comment-out-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 5 134217787] 0 "%d")) arg)))

(fset 'dup-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 5 3 timeout return 22] 0 "%d")) arg)))

(fset 'del-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("" 0 "%d")) arg)))

(fset 'template_block
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([123 37 32 37 125 f4] 0 "%d")) arg)))


(fset 'template_var
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([123 123 32 125 125 left left left 32 left 32 up down] 0 "%d")) arg)))

(fset 'copy-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 5 3 timeout] 0 "%d")) arg)))


(global-set-key (kbd "<f5> l") 'copy-line)

(global-set-key (kbd "<f5> i") 'ipdb)

(global-set-key (kbd "<C-f5>") 'revert-buffer)

(global-set-key (kbd "<f5> c") 'comment-out-line)

(global-set-key (kbd "<f5> d") 'dup-line)

(global-set-key (kbd "<f5> k") 'del-line)

(global-set-key (kbd "<f5> t") 'template_block)

(global-set-key (kbd "<f5> v") 'template_var)

(global-set-key [f6] 'multi-term)

(global-set-key [f8] 'dired-other-window)

(global-set-key (kbd "<C-tab>") 'other-window)

(global-set-key (kbd "<f5> r") 'magit-log-current-day)

(global-set-key (kbd "<f5> <f5>") 'restart-desktop)

(global-set-key (kbd "<C-f1>") 'delete-other-windows)

(global-set-key (kbd "<C-f2>") 'split-window-below)

(global-set-key (kbd "<C-f3>") 'split-window-right)

(global-set-key (kbd "<C-escape>") 'delete-window)
