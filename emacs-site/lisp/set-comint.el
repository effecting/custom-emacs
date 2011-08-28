;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'comint)

(setq comint-buffer-maximum-size 1024)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(provide 'set-comint)
