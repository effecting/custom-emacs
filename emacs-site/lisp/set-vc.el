;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'vc)

;; version control
(setq vc-checkout-carefully t)
;; should I really enable RCS?   all RCS-based are not really RCS
(setq vc-handled-backends '(RCS CVS SVN SCCS Bzr Git Hg Mtn Arch))

(provide 'set-vc)
