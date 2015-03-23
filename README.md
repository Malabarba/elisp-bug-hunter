##  ![hunter](hunter.png) Bug Hunter [![travis](https://secure.travis-ci.org/Bruce-Connor/elisp-bug-hunter.png?branch=master)](https://travis-ci.org/Bruce-Connor/elisp-bug-hunter?branch%3Dmaster)

`bug-hunter` is an Emacs library that finds the source of a bug inside any given `.el` file. 

## Usage Examples

1. If your Emacs init file signals an error during startup, but you
   don’t know why, simply issue

        M-x bug-hunter-init-file RET RET

   and `bug-hunter` will find it for you.

2. If Emacs starts up without errors but something is not working as
   it should, invoke the same command, but give it in an assertion:

         M-x bug-hunter-init-file RET (when issue-detected t) RET


3. You can also use `bug-hunter-file` to hunt in other files, or even
   `bug-hunter-hunt` to search in a list of expressions.

## Installation
It is part of Emacs and is available trough [GNU Elpa](https://elpa.gnu.org/packages/bug-hunter.html). To install, issue 

    M-x package-install RET bug-hunter

