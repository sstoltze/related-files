;;; related-files.el --- Work with @related file annotations  -*- lexical-binding:t -*-

;; Author: Sarah Stoltze <sstoltze@gmail.com>
;; Version: 1.0
;; Keywords: convenience, files, hypermedia
;; URL: https://github.com/sstoltze/related-files
;; Package-Requires: ((emacs "24.4"))


;;; Commentary:

;; This package provides support for working with related file annotations.
;; The annotations are described in more detail at https://github.com/synchronal/related-files.
;;
;; An annotation starts with @related, followed by one or more Markdown-style links consisting
;; of an arbitrary name (e.g., "test", "css", "header") and a path to the file:
;; @related [name](path)
;; The path is either relative to the current file (if it starts with a "."),
;; or to the project root otherwise.
;; The annotation ends with a newline character.
;; The following are all valid annotations.
;;
;; A very simple annotation:
;; @related [test](/src/related.test.js)
;;
;; Annotation lines can contain several related files
;; See @related [test](/src/related.test.js), [test](/src/other.test.js), and [css](/assets/style.css)
;;
;; And can span multiple lines
;; @related \
;;   [test](/test/core/foo_test.exs) \
;;   [test](/test/integration/bar_test.exs) \
;;   [sass](/assets/css/foo.sass)
;;
;; The package currently supports scanning for annotations and navigating between them.

;;; Code:

(defun related-files--line-end-respecting-backslash ()
  "Return the line-end-positon of nearest non-backslash terminated line."
  (let ((le 1))
    (while (and
            (< (line-end-position le)
               (point-max))
            (string-suffix-p "\\"
                             (string-trim-right
                              (buffer-substring-no-properties
                               (line-beginning-position le)
                               (line-end-position le)))))
      (setq le (1+ le)))
    le))

;;;###autoload
(defun related-files-list (&optional buffer-name)
  "Return an alist of (name . path) annotations for BUFFER-NAME, or current buffer."
  (interactive)
  (let* ((base-buffer (or buffer-name
                          (buffer-base-buffer)
                          (buffer-name)))
         (root-dir (cond ((and (fboundp 'project-current)
                               (fboundp 'project-root))
                          (project-root (project-current)))
                         ((fboundp 'vc-root-dir) (vc-root-dir))))
         (related-files (list)))
    (with-current-buffer base-buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((le (related-files--line-end-respecting-backslash))
                   (line (buffer-substring-no-properties (line-beginning-position)
                                                         (line-end-position le)))
                   (start-char 0))
              (when (string-match-p "@related" line)
                (while (string-match "\\[\\(.*?\\)\\](\\(.*?\\))" line start-char)
                  (let* ((name      (match-string 1 line))
                         (file-link (match-string 2 line))
                         (qualified-file-link (cond ((string= (substring file-link 0 1)
                                                              ".")
                                                     (concat (expand-file-name file-link)))
                                                    ((string= (substring file-link 0 1)
                                                              "/")
                                                     (concat root-dir (substring file-link 1)))
                                                    (t
                                                     (concat root-dir file-link)))))
                    (setq related-files (cons (cons (propertize name 'display (concat name " -> " file-link))
                                                    qualified-file-link)
                                              related-files)
                          start-char (match-end 0)))))
              (forward-line le))))))
    (reverse related-files)))

;;;###autoload
(defun related-files-find-related-file (&optional buffer-name)
  "Prompt to visit related files defined in BUFFER-NAME.
Defaults to current buffer."
  (interactive)
  (let* ((related-files (related-files-list buffer-name))
         (chosen-file (completing-read "Related files: " related-files nil nil)))
    (when chosen-file
      (let ((file-name (cdr (assoc-string chosen-file related-files))))
        (find-file file-name)))))


(provide 'related-files)
;;; related-files.el ends here
