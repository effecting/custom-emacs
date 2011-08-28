;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'ediff)

;; split side-by-side
(setq ediff-split-window-function 'split-window-horizontally)
;;(setq ediff-split-window-function 'split-window-vertically)

;; no new frames
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'set-ediff)
