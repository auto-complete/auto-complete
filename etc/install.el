(require 'cl)

(when (or (not (featurep 'auto-complete))
          (yes-or-no-p "You are trying to upgrade auto-complete within an existed Emacs which has loaded its older version.
It causes sometimes errors or installation fault. Are you sure? "))
  (let* ((basedir (file-name-directory (directory-file-name (file-name-directory load-file-name))))
         (default-dir "~/.emacs.d/")
         (todir (file-name-as-directory
                 (or (car command-line-args-left)
                     (read-directory-name "Install to: " default-dir default-dir))))
         (basedictdir (concat basedir "dict"))
         (todictdir (concat todir "ac-dict")))
    (cond
     ((not (file-directory-p basedir))
      (error "Base directory is not found"))
     ((or (= (length todir) 0)
          (not (file-directory-p todir)))
      (error "To directory is empty or not found"))
     (t
      (message "Installing to %s from %s" todir basedir)
      (add-to-list 'load-path basedir)
      (make-directory todictdir t)
      (loop for file in (directory-files basedir t "^.*\\.elc?$")
            do (copy-file file todir t))
      (loop for file in (directory-files basedictdir t "^[^\\.]")
            do (copy-file file todictdir t))
  
      (let ((msg (concat "Successfully installed!

Add the following code to your .emacs:

"
                         (if (and (not (member (expand-file-name todir) load-path))
                                  (not (member (concat (expand-file-name todir) "/") load-path)))
                             (format "(add-to-list 'load-path \"%s\")\n" (directory-file-name todir))
                           "")
                         "(require 'auto-complete-config)\n"
                         ;(format "(add-to-list 'ac-dictionary-directories \"%s\")\n" todictdir)
                         "(ac-config-default)\n")))
        (if noninteractive
            (progn (princ msg)
		   (princ "\n"))
          (switch-to-buffer "*Installation Result*")
          (erase-buffer)
          (insert msg)))))))
