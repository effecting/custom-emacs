;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; A custom configuration for loading EMMS just the way I want
;; and connecting to the Music Player Daemon.

(require 'emms)

(require (cond ((equal emms-version "3.0") 'set-emms-30-mpd)
               ((equal emms-version "2.1") 'set-emms-21)
               (t (error))))

(provide 'set-emms)
