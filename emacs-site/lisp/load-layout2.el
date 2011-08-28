;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; layout windows 2 row by 2 col, raster-scan order

;; original frame
(delete-other-frames)
(delete-other-windows)
(make-frame-command)
(make-frame-command)
(make-frame-command)

(fetchmail)
(server-start)
(mh-index-new-messages mh-new-messages-folders)

(require 'load-home)
(require 'load-work)

(other-frame 1)
(journalmonth)
(journaltoday)

