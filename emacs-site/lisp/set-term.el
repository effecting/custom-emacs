;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'term)

;; colors for 'term'
(setq ansi-color-for-comint-mode t)
(setq ansi-color-names-vector ["black" "red" "green" "yellow" "RoyalBlue" "magenta" "cyan" "white"])
(aset ansi-term-color-vector 5 "RoyalBlue3")
(aset ansi-term-color-vector 8 "AntiqueWhite")

(provide 'set-term)
