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

## Demo Video

* [YouTube](http://www.youtube.com/watch?v=rGVVnDxwJYE)
* [Python autocompletion for Emacs and Vim](http://youtu.be/FAi4LKgR6So)

## Install

`auto-complete` is available on [MELPA](http://melpa.org) and [MELPA-STABLE](http://stable.melpa.org)

You can install `auto-complete` with the following command.

<kbd>M-x package-install [RET] auto-complete [RET]</kbd>

This is [an example](http://www.emacswiki.org/emacs/ELPA) of `~/.emacs.d/init.el`.

	(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")
		("melpa" . "http://melpa.milkbox.net/packages/")))

[These lines will be executed](http://emacswiki.org/emacs/ELPA#toc5) after `(package-initialize)`.

	(add-hook 'after-init-hook
		(lambda ()
			(require 'auto-complete)
			(require 'auto-complete-config)
			(ac-config-default)
			))

## User Manual

[Auto-Complete User Manual](http://auto-complete.org/doc/manual.html)

## Development

* <http://github.com/auto-complete/auto-complete>

## Reporting Bugs

Visit
[Auto-Complete Issue Tracker](https://github.com/auto-complete/auto-complete/issues)
and create a new issue.

## License

This software is distributed under the term of GPLv3.
