;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; work documents
;; (find-file "~/doc/work_todo.txt")
;; (rename-buffer "work todo")
;; (other-window 1)
;; (ssh "erieX")

;; (require 'set-timeclock)
;; (timeclock-initialize)

;; (defun journal-reload ()
;;   (interactive)
;;   (setq jtemplmonth (format-time-string
;; "# -*- mode: org; mode: auto-fill; mode: smart-chars -*-
;; #
;; #+STARTUP:     logging showall hidestars
;; #+AUTHOR:      effecting2
;; #+SEQ_TODO:    MAYBE WAIT TODO PART VERIFY | DONE SKIP
;; #+OPTIONS:     H:5 toc:2 ^:nil
;; #
;; #+TEXT:        
;; #+TEXT:        
;; #

;; " (current-time)))

;;   (setq jtempltoday (format-time-string
;; "# -*- mode: org; mode: auto-fill; mode: smart-chars -*-
;; #
;; #+STARTUP:     logging showall hidestars
;; #+AUTHOR:      effecting2
;; #+SEQ_TODO:    TODO PART | DONE SKIP
;; #+OPTIONS:     H:5 toc:2 ^:nil
;; #
;; #+TEXT:        
;; #+TEXT:        
;; #

;; " (current-time)))
  
;;   (setq jtodaydir (concat journaldir "/" (format-time-string "%Y" (current-time))))
;;   (setq jmonthfile (concat jtodaydir "/" (format-time-string "%m" (current-time)) ".txt"))
;;   (setq jtodayfile (concat jtodaydir "/" (format-time-string "%m_%d" (current-time)) ".txt"))
;;   )

;; (journal-reload)

;; (defun mkjournalfile (filename template)
;;   (interactive)
;;   (find-file filename)
;;   (when (not (file-exists-p filename))
;;     (goto-char 0)
;;     (insert-string template)
;;     (save-buffer)
;;     (normal-mode)
;;     (vc-register)
;;     ))

;; (defun journalmonth ()
;;   (interactive)
;;   (journal-reload)
;;   (mkjournalfile jmonthfile jtemplmonth))

;; (defun journaltoday ()
;;   (interactive)
;;   (journal-reload)
;;   (mkjournalfile jtodayfile jtempltoday))

;; (defun open-journal ()
;;   (interactive)
;;   (journalmonth)
;;   (journaltoday))

(provide 'load-work)
