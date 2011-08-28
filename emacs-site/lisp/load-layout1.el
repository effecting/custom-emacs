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

(defun my-layout1 ()
  (delete-other-windows)
;  (split-window-horizontally)
  )

(my-layout1)

;; window 4
;; (mutt "132x11")

;; ;; daemons
(mairix_loop)
(fetchmail_loop)

; (gnuserv-start)
(server-start)

(require 'load-home)
;;(require 'load-work)
;;(mh-rmail)
(require 'anything)
(require 'skype)
(global-set-key (kbd "s-9") 'skype--anything-command)
(require 'set-bitlbee)
(bitlbee-start)
