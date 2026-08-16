;;; run-checkdoc.el --- Run Checkdoc for fp.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run Checkdoc noninteractively without relying on version-specific batch
;; entry points.

;;; Code:

(require 'checkdoc)

(find-file "fp.el")
(let ((checkdoc-autofix-flag nil))
  (checkdoc-current-buffer))

;;; run-checkdoc.el ends here
