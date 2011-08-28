;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'reftex)

;; reftex settings
(setq reftex-plug-into-AUCTeX t)
(setq reftex-texpath-environment-variables '(".:./secs:.."))
(setq reftex-bibpath-environment-variables '(".:./secs:.."))

(provide 'set-reftex)
