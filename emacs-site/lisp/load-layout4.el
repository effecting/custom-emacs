;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; layout windows 2 row by 2 col, raster-scan order

(delete-other-frames)
(delete-other-windows)

(defun my-layout4 ()
  (delete-other-windows)
  (split-window-horizontally)
  )

(my-layout4)

;; window 4
;; (mutt "132x11")

;; ;; daemons
(gnuserv-start)
(server-start)

(require 'load-home)
;;(mh-rmail)
