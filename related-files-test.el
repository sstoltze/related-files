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


(ert-deftest related-files-test ()
  (with-temp-buffer
    (insert-file-contents "./related-files-test.el")
    (let* ((related-files (sort (related-files-list)
                                #'(lambda (a b) (string< (car a)
                                                         (car b))))))
      (should (= (length related-files)
                 6))
      (should (equal (car related-files)
                     (cons "Makefile -> ./Makefile" (expand-file-name "./Makefile"))))
      (should (equal (cadr related-files)
                     (cons "non-relative-file -> /related-files.el"
                           (concat (related-files--project-root) "related-files.el"))))
      ;; Check that we insert a related-to entry in the other files
      (should (member (expand-file-name "./related-files.el")
                      (hash-table-keys (gethash (related-files--project-root)
                                                related-files--project-map))))
      (should (assoc "related-to"
                     (gethash (expand-file-name "./related-files.el")
                              (gethash (related-files--project-root)
                                       related-files--project-map))
                     ;; We have to switch the element order for string-prefix-p in the test-fn
                     (lambda (alist-elem s)
                       (string-prefix-p s alist-elem)))))))

(provide 'related-files-test)
;;; related-files-test.el ends here
