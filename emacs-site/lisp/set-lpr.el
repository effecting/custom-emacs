;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'lpr)

(setq effecting-lpr-command
      (cond ((eq window-system 'x) (cond ((executable-find "xfprint4") "xfprint4")
                                         ((executable-find "kprinter") "kprinter")
                                         ))
            (t nil)))

(when effecting-lpr-command
  (setq lpr-command effecting-lpr-command
        lpr-add-switches nil))
  
(provide 'set-lpr)
