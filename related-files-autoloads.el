;;; related-files-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from related-files.el

(autoload 'related-files-list "related-files" "\
Return an alist of (name . path) annotations for BUFFER-NAME, or current buffer.

(fn &optional BUFFER-NAME)" t)
(autoload 'related-files-find-related-file "related-files" "\
Prompt to visit related files defined in BUFFER-NAME.
Defaults to current buffer.

(fn &optional BUFFER-NAME)" t)
(autoload 'related-files-projects "related-files" "\
Return the list of projects in the project map.")
(autoload 'related-files-find-project "related-files" "\
Prompt to visit the projects that related-file knows about." t)
(register-definition-prefixes "related-files" '("related-files-"))


;;; End of scraped data

(provide 'related-files-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; related-files-autoloads.el ends here
