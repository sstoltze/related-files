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
;; @related [test](/related-files-test.el)
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

(require 'subr-x)
(declare-function string-remove-prefix "subr-x.el" (prefix string))

(defgroup related-files nil
  "Related file annotation support for Emacs."
  :group 'convenience
  :link '(url-link "https://github.com/sstoltze/related-files"))

(defvar related-files--project-map (make-hash-table :test 'equal))

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

(defun related-files--project-root ()
  "Get the root of the project, depending on what libraries are available."
  (expand-file-name
   (cond ((and (fboundp 'project-root) (fboundp 'project-current))
          (project-root (project-current)))
         ((fboundp 'vc-root-dir)
          (vc-root-dir))
         (t
          "~"))))

(defcustom related-files-link-string
  " -> "
  "The value to display between name and link in related file overview."
  :type 'string)

(defun related-files--build-cons (name relative-link qualified-link)
  "Build the assoc cons cell for the file NAME RELATIVE-LINK QUALIFIED-LINK."
  (cons (concat name
                related-files-link-string
                relative-link)
        qualified-link))

;;;###autoload
(defun related-files-list (&optional buffer-name)
  "Return an alist of (name . path) annotations for BUFFER-NAME, or current buffer."
  (interactive)
  (let* ((base-buffer (or buffer-name
                          (buffer-base-buffer)
                          (buffer-name)))
         (root-dir (related-files--project-root))
         (base-buffer-qualified-link (expand-file-name base-buffer))
         (base-buffer-project-link (string-remove-prefix root-dir
                                                         base-buffer-qualified-link))
         ;; Get hash of related files from project
         (project-related-files (gethash root-dir
                                         related-files--project-map
                                         (make-hash-table :test 'equal)))
         ;; Get existing list of related files for this buffer
         (related-files (gethash base-buffer-qualified-link
                                 project-related-files
                                 (list))))
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
                  (let* ((name         (match-string 1 line))
                         (project-link (match-string 2 line))
                         (qualified-link (expand-file-name
                                          (cond ((string= (substring project-link 0 1)
                                                          ".")
                                                 project-link)
                                                ((string= (substring project-link 0 1)
                                                          "/")
                                                 (concat root-dir (substring project-link 1)))
                                                (t
                                                 (concat root-dir project-link))))))
                    (setq related-files (cons (related-files--build-cons name project-link qualified-link)
                                              related-files)
                          start-char (match-end 0))
                    (puthash qualified-link
                             (delete-dups
                              (cons (related-files--build-cons "related-to" base-buffer-project-link base-buffer-qualified-link)
                                    (gethash qualified-link
                                             project-related-files
                                             (list))))
                             project-related-files))))
              (forward-line le))))))
    (puthash base-buffer-qualified-link
             (delete-dups related-files)
             project-related-files)
    (puthash root-dir
             project-related-files
             related-files--project-map)
    (gethash base-buffer-qualified-link
             project-related-files)))

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
