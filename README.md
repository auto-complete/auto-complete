# Auto-Complete

[![Build Status](https://secure.travis-ci.org/auto-complete/auto-complete.svg)](http://travis-ci.org/auto-complete/auto-complete)

An Intelligent auto-completion extension for Emacs

## What is Auto-Complete?

Auto-Complete is an intelligent auto-completion extension for
Emacs. It extends the standard Emacs completion interface and provides
an environment that allows users to concentrate more on their own
work.

## Features

* Visual interface
* Reduce overhead of completion by using statistic method
* Extensibility

## Screenshots

![](doc/ac.png "Auto Completion")

![](doc/ac-fuzzy.png "Fuzzy Completion")

![](doc/ac-isearch.png "Increamental Search")

## Install

`auto-complete` is available on [MELPA](https://melpa.org) and [MELPA-STABLE](https://stable.melpa.org)

You can install `auto-complete` with the following command.

<kbd>M-x package-install [RET] auto-complete [RET]</kbd>


## User Manual

[Auto-Complete User Manual](https://github.com/auto-complete/auto-complete/blob/master/doc/manual.md)


## Basic Configuration

```lisp
(add-hook 'after-init-hook
	  (lambda ()
	    (ac-config-default)
	    ))
```

## Development

* <http://github.com/auto-complete/auto-complete>

## Reporting Bugs

Visit
[Auto-Complete Issue Tracker](https://github.com/auto-complete/auto-complete/issues)
and create a new issue.

## License

This software is distributed under the term of GPLv3.
