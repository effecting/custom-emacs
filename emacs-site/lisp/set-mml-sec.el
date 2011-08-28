;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'mml-sec)

(setq mml-smime-verbose nil)

;; use EPA/EPG if available
(when (and (locate-library "epa") (locate-library "epg"))
  (require 'epg-config)
  (when (>= emacs-major-version 23)
    (require 'epa))
  (when (< emacs-major-version 23)
    (require 'epa-setup))
  (setq mml-smime-use 'epg))

(provide 'set-mml-sec)
