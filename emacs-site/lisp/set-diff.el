;; 
;; 

;; Authored by effecting2 in 2010.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'diff)

(setq diff-command (cond
                    ((eq window-system 'w32) "c:/cygwin-1.7/bin/diff.exe")
                    (t "diff")
                    ))

(provide 'set-diff)
