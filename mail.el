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

(provide 'mail)
;;; mail.el ends here
