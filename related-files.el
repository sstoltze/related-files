;;; related-files.el --- work with @related file annotations  -*- lexical-binding:t -*-

;; Author: Sarah Stoltze <sstoltze@gmail.com>
;; Version: 1.0
;; Keywords: convenience, files, hypermedia
;; URL: https://github.com/sstoltze/related-files



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

(defun sstoltze/line-end-respecting-backslash ()
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
(defun sstoltze/related-files ()
  "Return a list of (name . path) annotations for the current buffer."
  (interactive)
  (let* ((base-buffer (or (buffer-base-buffer)
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
            (let* ((le (sstoltze/line-end-respecting-backslash))
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
    related-files))

;;;###autoload
(defun sstoltze/find-related-file ()
  "Let's try this thing out."
  (interactive)
  (let* ((related-files (sstoltze/related-files))
         (chosen-file (completing-read "Related files: " related-files nil nil)))
    (when chosen-file
      (let ((file-name (cdr (assoc chosen-file related-files))))
        (find-file file-name)))))


(provide 'related-files)
;;; related-files.el ends here
