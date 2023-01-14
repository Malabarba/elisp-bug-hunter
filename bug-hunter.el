;;; bug-hunter.el --- Hunt down errors by bisecting elisp files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: https://github.com/Malabarba/elisp-bug-hunter
;; Version: 1.3.1
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
;; An Emacs library that finds the source of an error or unexpected
;; behavior inside an elisp configuration file (typically `init.el' or
;; `.emacs').
;;
;; Usage Examples
;; ==============
;;
;; Automated error hunting
;; ~~~~~~~~~~~~~~~~~~~~~~~
;;
;;   If your Emacs init file signals an error during startup, but you don’t
;;   know why, simply issue
;;   ,----
;;   | M-x bug-hunter-init-file RET e
;;   `----
;;   and The Bug Hunter will find it for you. Note that your `init.el' (or
;;   `.emacs') must be idempotent for this to work.
;;
;;
;; Interactive hunt
;; ~~~~~~~~~~~~~~~~
;;
;;   If Emacs starts up without errors but something is not working as it
;;   should, invoke the same command, but choose the interactive option:
;;   ,----
;;   | M-x bug-hunter-init-file RET i
;;   `----
;;   The Bug Hunter will start a separate Emacs instance several times, and
;;   then it will ask you each time whether that instance presented the
;;   problem you have. After doing this about 5--12 times, you’ll be given
;;   the results.
;;
;;
;; Assertion hunt
;; ~~~~~~~~~~~~~~
;;
;;   The Bug Hunter can also find your issue based on an assertion.
;;   Essentially, if you can write a code snippet that returns non-nil when
;;   it detects the issue, just provide this snippet as the assertion and
;;   the Bug Hunter will do the rest.
;;
;;   For example, let’s say there’s something in your init file that’s
;;   loading the `cl' library, and you don’t want that. You /know/ you’re
;;   not loading it yourself, but how can you figure out which external
;;   package is responsible for this outrage?
;;
;;   ,----
;;   | M-x bug-hunter-init-file RET a (featurep 'cl) RET
;;   `----
;;
;;   *That’s it!* You’ll be given a nice buffer reporting the results:
;;
;;   - Are you getting obscure errors when trying to open /".tex"/ files?
;;     - Don’t despair! Just use `(and (find-file "dummy.tex") nil)' as the
;;       assertion.
;;   - Did `ox-html' stop working due to some arcane misconfiguration?
;;     - Just write an assertion that does an export and checks the result.
;;   - Does some random command suddenly bind itself to `C-j' and you can’t
;;     figure out why?
;;     - `(eq (key-binding "\n") 'unwanted-command)' is the assertion for
;;       you!
;;
;;   Finally, you can also use `bug-hunter-file' to hunt in other files.
;;
;;
;; init.org and other literate-style configs
;; =========================================
;;
;; Please see the full Readme on http://github.com/Malabarba/elisp-bug-hunter

;;; Code:
(require 'seq)
(require 'cl-lib)

(defconst bug-hunter--interactive-explanation
  "You have asked to do an interactive hunt, here's how it goes.
1) I will start a new Emacs instance, which opens a new frame.
2) You will try to reproduce your problem on the new frame.
3) When you’re done, close that frame.
4) I will ask you if you managed to reproduce the problem.
5) We will repeat steps up to %s times, so hang tight!")

(defconst bug-hunter--assertion-reminder
  "Remember, the assertion must be an expression that returns
non-nil in your current (problematic) Emacs state, AND that
returns nil on a clean Emacs instance.
If you're unsure how to write an assertion, you can try the interactive
hunt instead, or see some examples in the Readme:
    https://github.com/Malabarba/elisp-bug-hunter"
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
          (error
           (if (string= (elt er 1) "Invalid modifier in string")
               `(bug-caught (invalid-modifier) ,line ,col)
             (error "Ran into an error we don't understand, please file a bug report: %S" er))))
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
  (redisplay)
  (apply #'message r))

(defun bug-hunter--report-user-error (&rest r)
  "Report the user has done something wrong.
R is passed to `bug-hunter--report-print'."
  (declare (indent 1))
  (apply #'bug-hunter--report-print r)
  (bug-hunter--report-print "\xc\n")
  (apply #'user-error r))

(defvar compilation-error-regexp-alist)
(defun bug-hunter--init-report-buffer (assertion steps)
  "Create and prepare the \"*Bug-Hunter Report*\" buffer.
Also add some descriptions depending on ASSERTION."
  (with-current-buffer (get-buffer-create "*Bug-Hunter Report*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (compilation-mode "Bug Hunt")
      (set (make-local-variable 'compilation-error-regexp-alist)
           '(comma))
      (pcase assertion
        (`interactive (insert (format bug-hunter--interactive-explanation (+ 2 steps))))))
    (current-buffer)))

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
      (invalid-modifier (concat "There's a string on this line with an invalid modifier."
                                "\n  A \"modifier\" is a \\ followed by a few characters."
                                "\n  For example, \\C-; is an invalid modifier."))
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
  (bug-hunter--report "\xc\n")
  `[,error ,line ,column ,expression])


;;; Execution functions
(defvar bug-hunter-header nil "Emacs Lisp sexp to add at the beginning of the file.")
(defvar bug-hunter-footer nil "Emacs Lisp sexp to add at the end of the file.")

(defun bug-hunter--print-to-temp (sexp)
  "Print SEXP to a temp file and return the file name."
  (let ((print-length nil)
        (print-level nil)
        (file (make-temp-file "bug-hunter")))
    (with-temp-file file
      (when bug-hunter-header (print bug-hunter-header (current-buffer)))
      (print sexp (current-buffer))
      (when bug-hunter-footer (print bug-hunter-footer (current-buffer))))
    file))

(defun bug-hunter--run-emacs (file &rest args)
  "Start an Emacs process to run FILE and return the output buffer.
ARGS are passed before \"-l FILE\"."
  (let ((out-buf (generate-new-buffer "*Bug-Hunter Command*"))
        (exec (file-truename (expand-file-name invocation-name
                                               invocation-directory))))
    (apply #'call-process exec nil out-buf nil
           (append args (list "-l" file)))
    out-buf))

(defun bug-hunter--run-form (form)
  "Run FORM with \"emacs -Q\" and return the result."
  (let ((file-name (bug-hunter--print-to-temp (list 'prin1 form))))
    (unwind-protect
        (with-current-buffer (bug-hunter--run-emacs file-name "-Q" "--batch")
          (goto-char (point-max))
          (forward-sexp -1)
          (prog1 (read (current-buffer))
            (kill-buffer (current-buffer))))
      (delete-file file-name))))

(defun bug-hunter--run-form-interactively (form)
  "Run FORM in a graphical instance and ask user about the outcome."
  (let ((file-name (bug-hunter--print-to-temp (list 'prin1 form))))
    (unwind-protect
        (bug-hunter--run-emacs file-name "-Q")
      (delete-file file-name))
    (y-or-n-p "Did you find the problem/bug in this instance (if you encounter some other issue, answer `n')? ")))

(defun bug-hunter--wrap-forms-for-eval (forms)
  "Return FORMS wrapped in initialization code."
  `(let ((server-name (make-temp-file "bug-hunter-temp-server-file")))
     (delete-file server-name)
     (if site-run-file (load site-run-file t t))
     (run-hooks 'before-init-hook)
     ,@forms
     (package-initialize)
     (run-hooks 'after-init-hook)))

(defun bug-hunter--run-and-test (forms assertion)
  "Execute FORMS in the background and test ASSERTION.
See `bug-hunter' for a description on the ASSERTION.

If ASSERTION is 'interactive, the form is run through
`bug-hunter--run-form-interactively'.  Otherwise, a slightly
modified version of the form combined with ASSERTION is run
through `bug-hunter--run-form'."
  (if (eq assertion 'interactive)
      (bug-hunter--run-form-interactively
       (bug-hunter--wrap-forms-for-eval forms))
    (bug-hunter--run-form
     `(condition-case er
          ,(append (bug-hunter--wrap-forms-for-eval forms)
                   (list assertion))
        (error (cons 'bug-caught er))))))



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
  (let ((expressions (unless (eq (car-safe rich-forms) 'bug-caught)
                       (mapcar #'car rich-forms)))
        (bug-hunter--estimate (ceiling (log (length rich-forms) 2))))
    ;; Prepare buffer, and make sure they've seen it.
    (switch-to-buffer (bug-hunter--init-report-buffer assertion bug-hunter--estimate))
    (when (eq assertion 'interactive)
      (read-char-choice "Please read the instructions above and type 6 when ready. " '(?6)))

    (cond
     ;; Check for errors when reading the init file.
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
          "No errors signaled after loading the entire file.
If you're looking for something that's not an error, use the
interactive hunt instead of the error hunt.  If you have some
elisp proficiency, you can also use the assertion hunt, see this
link for some examples:
    https://github.com/Malabarba/elisp-bug-hunter")
        (or assertion "")))

     ;; Make sure we're in a forest, not a volcano.
     ((bug-hunter--run-and-test nil assertion)
      (bug-hunter--report-user-error "Test failed.\n%s\n%s"
        (if assertion
            (concat "Assertion returned non-nil even on emacs -Q:"
                    bug-hunter--assertion-reminder)
          "Detected a signaled error even on emacs -Q. This could mean three
things things:
1. The problem happens inside `package-initialize'.
2. You wrote the assertion wrong.
3. There's something seriously wrong with your Emacs installation.")
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

(defconst bug-hunter--hunt-type-prompt
  "To bisect interactively, type i
To use automatic error detection, type e
To provide a lisp assertion, type a
=> ")

(defun bug-hunter--read-from-minibuffer ()
  "Read a list of expressions from the minibuffer.
Wraps them in a progn if necessary to always return a single
form.

The user may decide to not provide input, in which case
'interactive is returned.  Note, this is different from the user
typing `RET' at an empty prompt, in which case nil is returned."
  (pcase (read-char-choice (if (display-graphic-p)
                               bug-hunter--hunt-type-prompt
                             (replace-regexp-in-string "To bisect interactively,.*\n" ""
                                                       bug-hunter--hunt-type-prompt))
                           '(?i ?e ?a))
    (`?i
     (unless (display-graphic-p)
       (user-error "Sorry, but `interactive' bisection needs a graphical frame"))
     'interactive)
    (`?e nil)
    (_
     (require 'simple)
     (let ((exprs
            (with-temp-buffer
              ;; Copied from `read--expression'.
              (let ((minibuffer-completing-symbol t))
                (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'completion-at-point-functions
                                (if (fboundp 'elisp-completion-at-point)
                                    #'elisp-completion-at-point
                                  (with-no-warnings
                                    #'lisp-completion-at-point))
                                nil t)
                      (run-hooks 'eval-expression-minibuffer-setup-hook))
                  (insert
                   (read-from-minibuffer
                    "Provide an assertion.  This is a lisp expression that returns nil if (and only if) everything is fine:\n => "
                    nil read-expression-map nil 'read-expression-history))))
              (goto-char (point-min))
              (mapcar #'car (bug-hunter--read-buffer)))))
       (if (cdr exprs)
           (cons #'progn exprs)
         (car exprs))))))

;;;###autoload
(defun bug-hunter-file (file &optional assertion)
  "Bisect FILE while testing ASSERTION.
All sexps in FILE are read and passed to `bug-hunter-hunt' as a
list.  See `bug-hunter-hunt' for how to use ASSERTION."
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
ASSERTION."
  (interactive (list (bug-hunter--read-from-minibuffer)))
  (bug-hunter-file user-init-file assertion))

(provide 'bug-hunter)
;;; bug-hunter.el ends here
