;;; package --- Summary

;;; Commentary:

;;; Code:

(defun alezf-send-mail (recipient &optional subject body )
  "Sends mail to a RECIPIENT with a subject SUBJECT and a body BODY."
  (progn
    (mail)
    (mail-to) (insert recipient)
    (when subject
        (mail-subject) (insert subject))
    (when body
      (mail-text) (insert body))
    (mail-send)))


(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  ;; tomado de https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(provide 'mail)


;;; mail.el ends here
