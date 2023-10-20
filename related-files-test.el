;;; related-files-test.el -- tests for related-files -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'related-files)

;; A very simple annotation:
;; @related [tested-file](./related-files.el)
;;
;; Annotation lines can contain several related files
;; See @related [non-relative-file](/related-files.el), [readme](/README.org), and [Makefile](./Makefile)
;;
;; And can span multiple lines
;; @related \
;;   [one](./related-files-test.el) \
;;   unrelated text \
;;   [two](/.gitignore)


;; TODO: fix string-prefix for test runs
(ert-deftest related-files-test ()
  (with-temp-buffer
    (insert-file-contents "./related-files-test.el")
    (let* ((related-files (related-files-list)))
      (should (= (length related-files)
                 6))
      (should (equal (car related-files)
                     (cons "tested-file" (expand-file-name "./related-files.el"))))
      ;; TODO: fix project-root not being set for tests run from Makefile
      (should (equal (cadr related-files)
                     (cons "non-relative-file"
                           (concat (if (fboundp 'project-root)
                                       (project-root (project-current))
                                     "")
                                   "related-files.el")))))))

(provide 'related-files-test)
;;; related-files-test.el ends here
