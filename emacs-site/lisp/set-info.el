;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'info)

;; Why doesn't Emacs provide a simple function to prompt for a string and use it to lookup an info page directly
;; as if running 'info string' from the command line?

(defun Info-find (str)
  (interactive (list (read-string "Info: ")))
  (when (> (length str) 0)
    (info str)
    (rename-buffer (format "*info: %s*" str))
    ))

(provide 'set-info)

