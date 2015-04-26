;;; bug-hunter.el --- Hunt down errors in elisp files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: http://github.com/Malabarba/elisp-bug-hunter
;; Version: 0.1
;; Keywords: lisp
;; Package-Requires: ((seq "1.3") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; `bug-hunter' is an Emacs library that finds the source of an error or
;; unexpected behavior inside an elisp configuration file (typically
;; `init.el' or `.emacs').
;; 
;; 
;; Usage Examples
;; ==============
;; 
;;   If your Emacs init file signals an error during startup, but you don’t
;;   know why, simply issue
;;   ,----
;;   | M-x bug-hunter-init-file RET RET
;;   `----
;;   and `bug-hunter' will find it for you.
;; 
;;   If Emacs starts up without errors but something is not working as it
;;   should, invoke the same command, but give it in an assertion.
;;   Essentially, if you can write a snippet that detects the issue and
;;   returns non-nil, just provide this snippet as the assertion and the
;;   Bug Hunter will do a bisection search for you.
;; 
;;   For example, let’s say there’s something in your init file that’s
;;   loading the `cl' library, and you don’t want that. You /know/ you’re
;;   not loading it yourself, but how can you figure out which external
;;   package is responsible for this outrage?
;; 
;;   ,----
;;   | M-x bug-hunter-init-file RET (featurep 'cl) RET
;;   `----
;; 
;;   *That’s it!* You’ll be given a nice buffer reporting the results:
;; 
;;   - Are you getting obscure errors when trying to open /“.tex”/ files?
;;     Don’t despair! Just use `(find-file "dummy.tex")' as the assertion.
;;   - Did `ox-html' stop working due to some arcane misconfiguration? Just
;;     write an assertion that does an export and checks the result.
;;   - Does some random command suddenly bind itself to `C-j' and you can’t
;;     figure out why? `(eq (key-binding "\n") 'unwanted-command)' is the
;;     assertion for you!
;; 
;;   Finally, you can also use `bug-hunter-file' to hunt in other files.
;; 

;; Installation
;; ============
;; 
;;   Bug Hunter will be on Melpa shortly. For now, do the following:
;;   1. Open the `bug-hunter.el` file.
;;   2. Issue `M-x package-install-from-buffer`.


;;; Code:
(require 'seq)
(require 'cl-lib)

(defvar bug-hunter--assertion-reminder
  "Remember, the assertion must be an expression that returns
non-nil in your current (problematic) Emacs state, AND that
returns nil on a clean Emacs instance."
  "Printed to the user if they provide a bad assertion.")

(defvar bug-hunter--current-head nil
  "Current list of expressions under scrutiny.  Used for user feedback.
Used if the user aborts before bisection ends.")

(defvar bug-hunter--i 0
  "Current step of the bisection.  Used for user feedback.")
(defvar bug-hunter--estimate 0
  "Estimate on how many steps the bisection can take.  Used for user feedback.
This is the base 2 log of the number of expressions in the
file.")

(defvar bug-hunter--current-file nil
  "File currently being debugged.")

(defun bug-hunter--read-buffer ()
  "Return all sexps after point as a list."
  (let (out line col)
    (or (condition-case er
            ;; Looks hacky, but comes from `byte-compile-from-buffer'.
            (while (progn (while (progn (skip-chars-forward " \t\n\^l")
                                        (looking-at ";"))
                            (forward-line 1))
                          (not (eobp)))
              (setq line (line-number-at-pos (point)))
              (setq col (current-column))
              (push (list (read (current-buffer)) line col)
                    out)
              nil)
          (end-of-file `(bug-caught (end-of-file) ,line ,col))
          (invalid-read-syntax `(bug-caught ,er ,line ,col))
          (error (error "Ran into an error we don't understand, please file a bug report: %S" er)))
        (nreverse out))))

(defun bug-hunter--read-contents (file)
  "Return all sexps in FILE as a list."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (bug-hunter--read-buffer)))


;;; Reporting functions
(defun bug-hunter--report-print (&rest r)
  "Print information on the \"*Bug-Hunter Report*\" buffer.
R is passed to `format' and inserted."
  (with-current-buffer (get-buffer-create "*Bug-Hunter Report*")
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert "\n" (apply #'format r)))))

(defun bug-hunter--report (&rest r)
  "Report arbitrary information.
R is passed to `bug-hunter--report-print'."
  (declare (indent 1))
  (apply #'bug-hunter--report-print r)
  (apply #'message r))

(defun bug-hunter--report-user-error (&rest r)
  "Report the user has done something wrong.
R is passed to `bug-hunter--report-print'."
  (declare (indent 1))
  (apply #'bug-hunter--report-print r)
  (bug-hunter--report-print "\xc")
  (apply #'user-error r))

(defvar compilation-error-regexp-alist)
(defun bug-hunter--init-report-buffer ()
  "Create and prepare the \"*Bug-Hunter Report*\" buffer."
  (or (get-buffer "*Bug-Hunter Report*")
      (with-current-buffer (get-buffer-create "*Bug-Hunter Report*")
        (compilation-mode "Bug Hunt")
        (set (make-local-variable 'compilation-error-regexp-alist)
             '(comma))
        (current-buffer))))

(defun bug-hunter--pretty-format (value padding)
  "Return a VALUE as a string with PADDING spaces on the left."
  (with-temp-buffer
    (pp value (current-buffer))
    (goto-char (point-min))
    (let ((pad (make-string padding ?\s)))
      (while (not (eobp))
        (insert pad)
        (forward-line 1)))
    (buffer-string)))

(defun bug-hunter--report-error (error line column &optional expression)
  "Print on report buffer information about ERROR.
LINE and COLUMN are the coordinates where EXPRESSION started in
the file."
  (when line
    (bug-hunter--report "%S, line %s pos %s:"
      bug-hunter--current-file line column))
  (bug-hunter--report "  %s"
    (cl-case (car error)
      (end-of-file
       "There's a missing closing parenthesis, the expression on this line never ends.")
      (invalid-read-syntax
       (let ((char (cadr error)))
         (if (member char '("]" ")"))
             (concat "There's an extra " char
                     " on this position. There's probably a missing "
                     (if (string= char ")") "(" "[")
                     " before that.")
           (concat "There's a " char
                   " on this position, and that is not valid elisp syntax."))))
      (user-aborted
       (let* ((print-level 2)
              (print-length 15)
              (forms (cadr error))
              (size (length forms)))
         (concat "User aborted while testing the following expressions:\n"
                 (mapconcat (lambda (x) (bug-hunter--pretty-format x 4))
                            (if (< size 16) forms (seq-take forms 7))
                            "")
                 (when (> size 16)
                   (format "\n    ... %s omitted expressions ...\n\n"
                     (- size 14)))
                 (when (> size 16)
                   (mapconcat (lambda (x) (bug-hunter--pretty-format x 4))
                              (seq-drop forms (- size 7)) "")))))
      (assertion-triggered
       (concat "The assertion returned the following value here:\n"
               (bug-hunter--pretty-format (cadr error) 4)))
      (t (format "The following error was signaled here:\n    %S"
           error))))
  (when expression
    (bug-hunter--report "  Caused by the following expression:\n%s"
      (bug-hunter--pretty-format expression 4)))
  (bug-hunter--report "\xc")
  `[,error ,line ,column ,expression])


;;; Execution functions
(defun bug-hunter--run-form (form)
  "Run FORM with \"emacs -Q\" and return the result."
  (let ((out-buf (generate-new-buffer "*Bug-Hunter Command*"))
        (exec (file-truename (expand-file-name invocation-name
                                               invocation-directory)))
        (file-name (make-temp-file "bug-hunter")))
    (unwind-protect
        (let ((print-length nil)
              (print-level nil))
          (with-temp-file file-name
            (print (list 'prin1 form) (current-buffer)))
          (call-process exec nil out-buf nil
                        "-Q" "--batch" "-l" file-name)
          (with-current-buffer out-buf
            (goto-char (point-max))
            (forward-sexp -1)
            (prog1 (read (current-buffer))
              (kill-buffer (current-buffer)))))
      (delete-file file-name))))

(defun bug-hunter--run-and-test (forms assertion)
  "Execute FORMS in the background and test ASSERTION.
See `bug-hunter' for a description on the ASSERTION."
  (bug-hunter--run-form
   `(condition-case er
        (let ((server-name (make-temp-file "bug-hunter-temp-server-file")))
          (delete-file server-name)
          ,@forms
          (run-hooks 'after-init-hook)
          ,assertion)
      (error (cons 'bug-caught er)))))



;;; The actual bisection
(defun bug-hunter--split (l)
  "Split list L in two lists of same size."
  (seq-partition l (ceiling (/ (length l) 2.0))))

(defun bug-hunter--bisect (assertion safe head &optional tail)
  "Implementation used by `bug-hunter--bisect-start'.
ASSERTION is received by `bug-hunter--bisect-start'.
SAFE is a list of forms confirmed to not match the ASSERTION,
HEAD is a list of forms to be tested now, and TAIL is a list
which will be inspected if HEAD doesn't match ASSERTION."
  (message "Testing: %s/%s" (cl-incf bug-hunter--i) bug-hunter--estimate)
  ;; Used if the user quits.
  (setq bug-hunter--current-head head)
  (let ((ret-val (bug-hunter--run-and-test (append safe head) assertion)))
    (cond
     ((not tail)
      (cl-assert ret-val nil)
      (vector (length safe) ret-val))
     ;; Issue in the head.
     ((and ret-val (< (length head) 2))
      (vector (length safe) ret-val))
     (ret-val
      (apply #'bug-hunter--bisect
        assertion
        safe
        (bug-hunter--split head)))
     ;; Issue in the tail.
     (t (apply #'bug-hunter--bisect
          assertion
          (append safe head)
          ;; If tail has length 1, we already know where the issue is,
          ;; but we still do this to get the return value.
          (bug-hunter--split tail))))))

(defun bug-hunter--bisect-start (forms assertion)
  "Run a bisection search on list of FORMS using ASSERTION.
Returns a vector [n value], where n is the position of the first
element in FORMS which trigger ASSERTION, and value is the
ASSERTION's return value.

If ASSERTION is nil, n is the position of the first form to
signal an error and value is (bug-caught . ERROR-SIGNALED)."
  (let ((bug-hunter--i 0)
        (bug-hunter--estimate (ceiling (log (length forms) 2)))
        (bug-hunter--current-head nil))
    (condition-case-unless-debug nil
        (apply #'bug-hunter--bisect assertion nil (bug-hunter--split forms))
      (quit `[nil (bug-caught user-aborted ,bug-hunter--current-head)]))))


;;; Main functions
(defun bug-hunter-hunt (rich-forms assertion)
  "Bisect RICH-FORMS using ASSERTION.
RICH-FORMS is a list with elements of the form: (EXPR LINE COL)
    EXPR is an elisp expression.  LINE and COL are the coordinates
    in `bug-hunter--current-file' where the expression starts.
It is expected that one of EXPR is either throwing an error or
causing some undesirable effect (which triggers ASSERTION).

ASSERTION is either nil or an expression.
    If nil, EXPRs are bisected until we find the first one that
    throws errors.
    If it is an expression, EXPRs are bisected by testing
    ASSERTION.  It should return nil if all is fine (e.g. if used
    with \"emacs -Q\"), and should return non-nil when a problem
    is detected.

Bug hunter will refuse to hunt if (i) an error is signaled or the
assertion is triggered while running emacs -Q, or (ii) no errors
are signaled and the assertion is not triggered after all EXPRs
are evaluated."
  (pop-to-buffer (bug-hunter--init-report-buffer))
  (let ((expressions (unless (eq (car-safe rich-forms) 'bug-caught)
                       (mapcar #'car rich-forms))))
    (cond
     ((not expressions)
      (apply #'bug-hunter--report-error (cdr rich-forms))
      (apply #'vector (cdr rich-forms)))

     ;; Make sure there's a bug to hunt.
     ((progn (bug-hunter--report "Doing some initial tests...")
             (not (bug-hunter--run-and-test expressions assertion)))
      (bug-hunter--report-user-error "Test failed.\n%s\n%s"
        (if assertion
            (concat "The assertion returned nil after loading the entire file.\n"
                    bug-hunter--assertion-reminder)
          "No errors signaled after loading the entire file. If you're
looking for something that's not an error, you need to provide an
assertion. See this link for some examples:
    https://github.com/Bruce-Connor/elisp-bug-hunter")
        (or assertion "")))

     ;; Make sure we're in a forest, not a volcano.
     ((bug-hunter--run-and-test nil assertion)
      (bug-hunter--report-user-error "Test failed.\n%s\n%s"
        (if assertion
            (concat "Assertion returned non-nil even on emacs -Q:"
                    bug-hunter--assertion-reminder)
          "Detected a signaled error even on emacs -Q. I'm sorry, but there
is something seriously wrong with your Emacs installation.
There's nothing more I can do here.")
        (or assertion "")))

     (t
      ;; Perform the actual hunt.
      (bug-hunter--report "Initial tests done. Hunting for the cause...")
      (let* ((result (bug-hunter--bisect-start expressions assertion)))
        (if (not result)
            (bug-hunter--report-user-error "No problem was found, despite our initial tests.\n%s"
              "I have no idea what's going on.")
          (let* ((pos (elt result 0))
                 (ret (elt result 1))
                 (linecol (when pos (cdr (elt rich-forms pos))))
                 (expression (when pos (elt expressions pos))))
            (if (eq (car-safe ret) 'bug-caught)
                (bug-hunter--report-error
                 (cdr ret) (car linecol) (cadr linecol) expression)
              (bug-hunter--report-error
               (list 'assertion-triggered ret)
               (car linecol) (cadr linecol) expression)))))))))

(defun bug-hunter--read-from-minibuffer ()
  "Read a list of expressions from the minibuffer.
Wraps them in a progn if necessary."
  (require 'simple)
  (let ((exprs
         (with-temp-buffer
           ;; Copied from `read--expression'.
           (let ((minibuffer-completing-symbol t))
             (minibuffer-with-setup-hook
                 (lambda ()
                   (add-hook 'completion-at-point-functions
                             #'elisp-completion-at-point nil t)
                   (run-hooks 'eval-expression-minibuffer-setup-hook))
               (insert
                (read-from-minibuffer
                 "Expression that returns nil if all is well (optional): "
                 nil read-expression-map nil 'read-expression-history))))
           (goto-char (point-min))
           (mapcar #'car (bug-hunter--read-buffer)))))
    (if (cdr exprs)
        (cons #'progn exprs)
      (car exprs))))

;;;###autoload
(defun bug-hunter-file (file &optional assertion)
  "Bisect FILE while testing ASSERTION.
All sexps in FILE are read and passed to `bug-hunter-hunt' as a
list.  See `bug-hunter-hunt' for how to use assertion."
  (interactive
   (list
    (read-file-name "File to bisect: "
                    (file-name-directory (or (buffer-file-name) "./"))
                    nil t
                    (file-name-nondirectory (or (buffer-file-name) "./")))
    (bug-hunter--read-from-minibuffer)))
  (let ((bug-hunter--current-file file))
    (bug-hunter-hunt (bug-hunter--read-contents file) assertion)))

;;;###autoload
(defun bug-hunter-init-file (&optional assertion)
  "Test ASSERTION throughout `user-init-file'.
All sexps inside `user-init-file' are read and passed to
`bug-hunter-hunt' as a list.  See `bug-hunter-hunt' for how to use
assertion."
  (interactive (list (bug-hunter--read-from-minibuffer)))
  (bug-hunter-file user-init-file assertion))

(provide 'bug-hunter)
;;; bug-hunter.el ends here
