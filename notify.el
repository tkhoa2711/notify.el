;;; notify.el --- A minimal library for notification in emacs lisp
;;
;; Copyright (C) 2015-2016 Khoa Le
;;
;; Author: Khoa Le <tkhoa2711@gmail.com
;; Created: 18 Aug 2015
;; URL: https://github.com/tkhoa2711/notify.el
;; Keywords: utility, notification, alert
;; Version: 1.0
;; Package-Requires: ()
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This library provide minimal functionality for notification/alert.
;; Currently, Emacs minibuffer is used by default. Growl is also supported.
;;
;; To enable:
;;
;;    (require 'notify)
;;
;; Then, a notification can be sent by simply calling:
;;
;;    (notify "title" "message")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; notify.el ends here
