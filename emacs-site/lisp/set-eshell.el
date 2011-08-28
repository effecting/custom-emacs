;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; make eshell's ls inherit from dired.
(require 'eshell)
(require 'dired)

(custom-set-faces
 '(eshell-ls-directory
   ((t :inherit dired-directory)))
 '(eshell-ls-symlink
   ((t :inherit dired-symlink)))
 '(eshell-ls-backup
   ((t :inherit dired-ignored))))

;; do not prompt about saving history.
(setq eshell-save-history-on-exit t)

(provide 'set-eshell)
