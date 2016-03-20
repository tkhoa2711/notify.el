;;; test-helper.el --- F: Test helpers  -*- lexical-binding: t; -*-

(require 'f)
(require 'ert)

;; load the package file
(require 'notify (f-expand "notify" (f-parent (f-parent (f-this-file)))))

(provide 'test-helper)

;;; test-helper.el ends here
