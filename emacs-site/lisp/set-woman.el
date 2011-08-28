;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

(require 'woman)

(setq woman-cache-level 1)
(setq woman-use-own-frame nil)

(defun woman-this ()
  (interactive)
  (woman-find-file buffer-file-name))

(provide 'set-woman)
