#+OPTIONS: toc:nil num:nil
#+TITLE: [[file:hunter.png]] The Bug Hunter [[https://travis-ci.org/Malabarba/elisp-bug-hunter][file:https://travis-ci.org/Malabarba/elisp-bug-hunter.svg?branch=master]]

/Automatically debug and bisect your init (.emacs) file!/

The Bug Hunter is an Emacs library that finds the source of an error
or unexpected behavior inside an elisp configuration file (typically
~init.el~ or ~.emacs~).

[[file:hunter-screencast.gif]]

* Usage Examples

** Automated error hunting
If your Emacs init file signals an error during startup, but you don’t
know why, simply issue
#+BEGIN_SRC text
M-x bug-hunter-init-file RET e
#+END_SRC
and The Bug Hunter will find it for you.  Note that your ~init.el~
(or ~.emacs~) must be idempotent for this to work.

** Interactive hunt

If Emacs starts up without errors but something is not working as it
should, invoke the same command, but choose the interactive option:
#+BEGIN_SRC text
M-x bug-hunter-init-file RET i
#+END_SRC
The Bug Hunter will start a separate Emacs instance several times, and
then it will ask you each time whether that instance presented the
problem you have. After doing this about 5--12 times, you’ll be given
the results.

** Assertion hunt

The Bug Hunter can also find your issue based on an assertion.
Essentially, if you can write a code snippet that returns non-nil when
it detects the issue, just provide this snippet as the assertion and
the Bug Hunter will do the rest.

For example, let’s say there’s something in your init file that’s
loading the ~cl~ library, and you don’t want that. You /know/ you’re
not loading it yourself, but how can you figure out which external
package is responsible for this outrage?

#+BEGIN_SRC text
M-x bug-hunter-init-file RET a (featurep 'cl) RET
#+END_SRC

*That’s it!* You’ll be given a nice buffer reporting the results:

[[file:cl-example.png]]
- Are you getting obscure errors when trying to open /“.tex”/ files?
  - Don’t despair! Just use ~(and (find-file "dummy.tex") nil)~ as the assertion.
- Did ~ox-html~ stop working due to some arcane misconfiguration?
  - Just write an assertion that does an export and checks the result.
- Does some random command suddenly bind itself to ~C-j~ and you can’t figure out why?
  - ~(eq (key-binding "\n") 'unwanted-command)~ is the assertion for you!

Finally, you can also use ~bug-hunter-file~ to hunt in other files.

* Installation
The Bug Hunter is available from [[https://elpa.gnu.org/packages/bug-hunter.html][GNU Elpa]] to all Emacs versions since
~24.1~. To install, just issue

#+BEGIN_SRC text
M-x package-install RET bug-hunter
#+END_SRC

* init.org and other literate-style configs

Some people (me included) like to organize their init files by
writting it in ~org-mode~ instead of Emacs-Lisp. This usually involves
adding something like this to ~init.el~,
#+BEGIN_SRC emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maybe some code up here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)
(org-babel-tangle-file "~/.emacs.d/org-init.org"
                       "~/.emacs.d/org-init.el")
(load "~/.emacs.d/org-init.el")
#+END_SRC

At first, this makes the Bug-Hunter essentially useless, for it will
do the hunting in ~init.el~ instead of the much more extensive
~org-init.el~. The name of the second file (~org-init.el~) will vary,
but the point is the same. But fear not! There’s a simple solution:

1. If you have any code above the call to ~org-babel-tangle-file~, copy that to the top of ~org-init.el~ (or whatever is the name of your tangled file). This includes that ~(require 'org)~ over there.
2. Invoke ~M-x~ ~bug-hunter-file~ (instead of ~bug-hunter-init-file~). It will ask you which file to debug, and you need to point it to your tangled output file ~org-init.el~.
