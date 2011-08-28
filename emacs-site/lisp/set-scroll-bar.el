;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'scroll-bar)

;; if we use scroll bar, make right-side
(when scroll-bar-mode (set-scroll-bar-mode 'right))

(provide 'set-scroll-bar)
