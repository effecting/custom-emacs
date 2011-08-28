;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'mml2015)

(setq mml2015-verbose nil)
(setq mml2015-encrypt-to-self t)

;; really enforce use of EPG/EPA if available
(when (locate-library "epg-config")
  (require 'epg-config)
  (setq mml2015-use 'epg))

(provide 'set-mml2015)
