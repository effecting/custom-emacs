;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(defvar laptop nil)

(if laptop
  (require 'conf-laptop))

;; typical startup actions
(require 'set-info)
;(Info-find "org")
;(Info-find "erc")
;(Info-find "elisp")
;(Info-find "ledger")
;(when (boundp 'custom-dir)
;  (find-file custom-dir))
;(when (file-exists-p "~/doc/personal_todo.txt")
;  (find-file "~/doc/personal_todo.txt"))
;(when (file-exists-p "~/svn_work/personal/finances/ledger.dat")
;  (find-file "~/svn_work/personal/finances/ledger.dat"))
(other-window 1)

(provide 'load-home)
