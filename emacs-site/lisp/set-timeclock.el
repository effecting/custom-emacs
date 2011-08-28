;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(provide 'set-timeclock)
(require 'timeclock)
(require 'timeclock-x)

(setq timeclock-directory
      (convert-standard-filename
       (cond
        ((file-exists-p "~/svn_work/timeclock") "~/svn_work/timeclock")
        )))

(setq timeclock-file
      (convert-standard-filename (concat timeclock-directory "/timelog.txt")))
(setq timeclock-default-log
      (convert-standard-filename (concat timeclock-directory "/default.log")))

(setq timeclock-workday (* 8.25 60 60))

;(timeclock-modeline-display 1)
