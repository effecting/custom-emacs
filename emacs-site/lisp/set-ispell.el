;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'ispell)

;; progs
(setq ispell-program-name
      (or (locate-file "aspell"   exec-path exec-suffixes 'file-executable-p)
          (locate-file "ispell"   exec-path exec-suffixes 'file-executable-p)
          (locate-file "hunspell" exec-path exec-suffixes 'file-executable-p)
          "ispell"))

(provide 'set-ispell)
