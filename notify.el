;;; notify.el - A minimal library for notification in elisp

(defvar notify-function 'notify-minibuffer
  "Function to send notification.
Set this variable to your preferred notification function.")

(defvar notify-growl-executable 
  (pcase system-type
   (`windows-nt
    "growlnotify.com")
   (`darwin
    "growlnotify")
   (`gnu/linux
    "gol"))
  "Set the path to growlnotify if it isn't executable within your PATH environment")

(defun notify (title body)
  "Send notification with title and body."
  (funcall notify-function title body))

(defun notify-minibuffer (title body)
  "Show TITLE and BODY in the minibuffer."
  (message "Notification: [%s] %s" title body))

(defun notify-growl (title body)
  "Send message with TITLE and BODY via Growl."
  (start-process "growl"
		 "growl"
		 notify-growl-executable
		 "-t"
		 title
		 "-m"
		 body))

(provide 'notify)

;;; notify.el ends here
