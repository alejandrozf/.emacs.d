(defun create-asdf-config ()
  (when (eq system-type 'gnu/linux)
    (let* ((route1 (expand-file-name "~/.config/common-lisp/source-registry.conf.d/"))
           (command1 (concat "mkdir -p " route1))
           (command2 (concat "mkdir -p " (expand-file-name "~/lisp/")))
           (config-file (concat route1 "asdf.conf"))
           (config-content (concat "(:tree "
                                   (format "\"%s\")" (expand-file-name "~/projects")))))
      (unless (file-exists-p config-file)
        (shell-command command1)
        (shell-command command2)
        ;; create asdf config file with asdf expected contents
        (write-region "" nil config-file)
        (with-temp-file config-file (insert config-content))))))

(create-asdf-config)
