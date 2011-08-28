;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'nroff-mode)
(require 'set-woman)

(defadvice nroff-mode (after my-nroff-mode activate)
  (local-set-key (kbd "C-c p") 'woman-this)
  (local-set-key (kbd "C-x p") 'woman-this)
  )

(provide 'set-nroff-mode)
