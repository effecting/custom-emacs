(defun list-autoloads ()
  (interactive)
  (switch-to-buffer "*Autoloads*")
  (erase-buffer)
  (mapatoms
   (lambda (F)
     (and (fboundp F)
          (eq (car-safe (symbol-function F)) 'autoload)
          (insert (format "%-40s [%s]\n"
                          (symbol-name F)
                          (locate-file (symbol-file F)
                                       load-path
                                       '(".el" "" ".elc")) )) )) ))
