;;; package --- Summary

;;; Commentary:

;;; Code:

(defun alezf-send-mail (recipient &optional subject body attachment)
  "Sends mail to a RECIPIENT with an optional SUBJECT, BODY and ATTACHMENT."
  (progn
    (mail)
    (mail-to) (insert recipient)
    (when subject
        (mail-subject) (insert subject))
    (when body
      (mail-text) (insert body))
    (when attachment
      (mail-add-attachment attachment))
    (mail-send-and-exit)))


(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  ;;TODO: mover esta función a otro módulo
  ;; tomado de https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(provide 'mail)

;;; mail.el ends here
