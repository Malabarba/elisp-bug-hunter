(unless (bound-and-true-p package--initialized)
  (setq
   package-user-dir (expand-file-name
                     (format ".cask/%s/elpa" emacs-version)
                     (file-name-directory load-file-name)))

  (package-initialize))

(require 'ert)
(require 'cl)
(require 'bug-hunter)
(fset 'bug-hunter--report #'ignore)
;; (fset 'bug-hunter--report-end #'ignore)

(ert-deftest bug-hunter-test ()
  (should
   (equal [5 2 void-variable not-defined]
          (bug-hunter-hunt
           '(((setq test 1) 3 0)
             ((setq test 2) 4 1)
             (not-defined 5 2))
           nil)))
  (should
   (equal [2 11 assertion-triggered t]
          (bug-hunter-hunt
           '(((setq test0 0) 0 9)
             ((setq test1 1) 1 10)
             ((setq test2 2) 2 11))
           '(ignore-errors (> test2 test1))))))

(ert-deftest bug-hunter-test-nobug ()
  (should-error (bug-hunter-hunt
                 '(((setq test 1) 0 1)
                   ((setq test 2) 0 1))
                 nil)))

(ert-deftest bug-hunter-test-volcano ()
  (should-error
   (bug-hunter-hunt nil 'not-defined)))

(ert-deftest bug-hunter-looong-hunt ()
  (let* ((size 30)
         (forms (make-list size '((setq dummy 1) 12 90))))
    (dotimes (n size)
      (setcar (elt forms (- size n 1)) 'not-defined)
      (should
       (equal [12 90 void-variable not-defined]
              (bug-hunter-hunt forms nil)))))
  (let* ((size 8)
         (forms (make-list size '(setq dummy 1))))
    (dotimes (n size)
      (let ((pos (- size n 1)))
        (setf (elt forms pos) 'not-defined)
        (should
         (equal (vector pos '(bug-caught void-variable not-defined))
                (bug-hunter--bisect-start forms nil)))))))

(ert-deftest bug-hunter-reader-error-test ()
  (let ((file (expand-file-name "bug-hunter-test-dummy-file"
                                default-directory)))
    (with-temp-file file
      (insert "(setq useless 1)\n#\n(setq useless 1)\n"))
    (should
     (equal (bug-hunter-file file nil)
            [2 0 invalid-read-syntax "#"]))
    (should
     (equal '(bug-caught 2 0 invalid-read-syntax "#")
            (bug-hunter--read-contents file)))
    (with-temp-file file
      (insert "(setq useless 1)\n)\n(setq useless 1)\n"))
    (should
     (equal '(bug-caught 2 0 invalid-read-syntax ")")
            (bug-hunter--read-contents file)))
    (with-temp-file file
      (insert "(setq useless 1)\n(\n(setq useless 1)\n"))
    (should
     (equal '(bug-caught 2 0 end-of-file)
            (bug-hunter--read-contents file)))))
