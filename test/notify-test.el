;;; notify-test.el --- Test for notify.el

;; Copyright (C) 2015-2016 Khoa Le

;; Author: Khoa Le <ltkhoa2711@gmai.com>
;; URL: http://github.com/notify.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(ert-deftest notify-function-defined ()
  "Ensure these functions are defined."
  (should (boundp 'notify-function))
  (should (fboundp 'notify))
  (should (fboundp 'notify-minibuffer))
  (should (fboundp 'notify-growl)))

(ert-deftest notify-calling ()
  "This call should execute successfully by default."
  (notify "test-title" "test-body"))

(provide 'notify-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; notify-test.el ends here
