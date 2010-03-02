(require 'cl)

(let* ((basedir (file-name-directory (directory-file-name (file-name-directory load-file-name))))
       (todir (read-file-name "Install to: " "~/.emacs.d"))
       (basedictdir (concat basedir "/dict"))
       (todictdir (concat todir "/ac-dict")))
  (make-directory todictdir t)
  (loop for file in (directory-files basedir t "^.*\\.el$")
        do (byte-compile-file file))
  (loop for file in (directory-files basedir t "^.*\\.elc?$")
        do (copy-file file todir t))
  (loop for file in (directory-files basedictdir t "^[^\\.]")
        do (copy-file file todictdir t))
  
  (switch-to-buffer "*Installation Result*")
  (erase-buffer)
  (insert "Successfully installed!

Add the following code to your .emacs:

"
          (if (and (not (member (expand-file-name todir) load-path))
                   (not (member (concat (expand-file-name todir) "/") load-path)))
              (format "(add-to-list 'load-path \"%s\")\n" todir)
            "")
          (format "(add-to-list 'ac-dictionary-directories \"%s\")\n" todictdir)
          "(require 'auto-complete-config)
(ac-config-default)
"))
