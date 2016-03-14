;;; notify.el - A minimal library for notification in elisp

(require 'eieio)

(defvar notify-function 'notify-minibuffer
  "Function to send notification.
Set this variable to your preferred notification function.")

(defvar notify-growl-executable 
  (case system-type
   ('windows-nt
    "growlnotify.com")
   ('darwin
    "growlnotify")
   ('gnu/linux
    "gol"))
  "Set the path to growlnotify if it isn't executable within your PATH environment")

(defun notify (title body)
  "Send notification with TITLE and BODY."
  (funcall notify-function title body))

(defun notify-minibuffer (title body)
  "Show TITLE and BODY in the minibuffer."
  (message "Notification: [%s] %s" title body))

(defclass os-type () ()
  "The type of current OS.")

(defclass windows-os (os-type) ())
(defclass unix-os (os-type) ())

(defconst *os-type*
  (case system-type
    ('windows-nt (make-instance windows-os))
    (:else (make-instance unix-os))))

(defmethod notify-growl% ((os-type windows-os) title body)
  "Notification using Growl."
  (start-process "notify-growl"
                 "*notify-growl*"
                 notify-growl-executable
                 (concat "/t:" title)
                 body))

(defmethod notify-growl% ((os-type unix-os) title body)
  "Notification using Growl on UNIX-like platform."
  (start-process "notify-growl"
                 "*notify-growl*"
                 notify-growl-executable
                 "-t"
                 title
                 body))

(defun notify-growl (title body)
  "Send a Growl notification with TITLE and BODY."
  (notify-growl% *os-type* title body))

(provide 'notify)

;;; notify.el ends here
