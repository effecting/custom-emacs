;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; Configurations pertaining exclusively to the X11 UI.

;; don't let Emacs try to minimize itself if I accidentally hit C-z
;; I'm not even sure my tiling window manager supports minimize (or unminimize)
(global-unset-key (kbd "C-z"))

(provide 'conf-ui-x11)
