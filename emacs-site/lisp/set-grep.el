;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'grep)

(setq find-program (cond
                    ((eq window-system 'w32) "gnufind")
                    (t "find")
                    ))
;; (setq xargs-program (cond
;;                     ((eq window-system 'w32) "\"c:\\Program Files\\GnuWin32\\bin\\xargs.exe\"")
;;                     (t "xargs")
;;                     ))
;; (setq grep-program (cond
;;                     ((eq window-system 'w32) "\"c:\\Program Files\\GnuWin32\\bin\\grep.exe\"")
;;                     (t "grep")
;;                     ))

(setq grep-find-command (concat find-program " . -type f \"!\" -regex \".*/\\(\\.svn\\|.git\\|.bzr\\|CVS\\|RCS\\)/.*\" -print0 | " xargs-program " -0 -e " grep-program " -nH -e "))

(provide 'set-grep)
