(unless (bound-and-true-p package--initialized)
  (setq
   package-user-dir (expand-file-name
                     (format ".cask/%s/elpa" emacs-version)
                     (file-name-directory load-file-name)))

  (package-initialize))

(require 'ert)
(require 'cl)
(require 'bug-hunter)

(ert-deftest bug-hunter-test ()
  (should
   (equal [2 (error void-variable not-defined)]
          (bug-hunter-hunt
           '((setq test 1)
             (setq test 2)
             not-defined)
           nil)))
  (should
   (equal [2 t]
          (bug-hunter-hunt
           '((setq test0 0)
             (setq test1 1)
             (setq test2 2))
           '(ignore-errors (> test2 test1)))))
  (should-error (bug-hunter-hunt
                 '((setq test 1)
                   (setq test 2))
                 nil))
  (should-error
   (bug-hunter-hunt nil 'not-defined)))

(ert-deftest bug-hunter-looong-hunt ()
  (let* ((submax 30)
         (size (* 3 submax))
         (forms (make-list size '(setq dummy 1))))
    (dotimes (n submax)
      (let ((pos (- size (* 3 n) 1)))
        (setf (elt forms pos) 'not-defined)
        (should
         (equal (vector pos '(error void-variable not-defined))
                (bug-hunter-hunt forms nil)))))))
