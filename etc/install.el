(require 'cl)

(let* ((basedir (file-name-directory (directory-file-name (file-name-directory load-file-name))))
       (todir (read-file-name "Install to: " "~/.emacs.d/" "~/.emacs.d/"))
       (basedictdir (concat basedir "/dict"))
       (todictdir (concat todir "/ac-dict")))
  (add-to-list 'load-path basedir)
  (make-directory todictdir t)
  (loop for file in (directory-files basedir t "^.*\\.el$")
        do (byte-compile-file file))
  (loop for file in (directory-files basedir t "^.*\\.elc?$")
        do (copy-file file todir t))
  (loop for file in (directory-files basedictdir t "^[^\\.]")
        do (copy-file file todictdir t))
  
  (let ((msg (concat "Successfully installed!

Add the following code to your .emacs:

"
                     (if (and (not (member (expand-file-name todir) load-path))
                              (not (member (concat (expand-file-name todir) "/") load-path)))
                         (format "(add-to-list 'load-path \"%s\")\n" todir)
                       "")
                     "(require 'auto-complete-config)\n"
                     (format "(add-to-list 'ac-dictionary-directories \"%s\")\n" todictdir)
                     "(ac-config-default)\n")))
    (if noninteractive
        (princ-list msg)
      (switch-to-buffer "*Installation Result*")
      (erase-buffer)
      (insert msg))))
