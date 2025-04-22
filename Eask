;; -*- mode: eask; lexical-binding: t -*-

(package "auto-complete"
         "1.5.1"
         "Auto Completion for GNU Emacs")

(website-url "https://github.com/auto-complete/auto-complete")
(keywords "completion" "convenience")

(package-file "auto-complete.el")

(files "auto-complete-config.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "25.1")
(depends-on "popup")
(depends-on "fuzzy")

(development
 (depends-on "ert"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
