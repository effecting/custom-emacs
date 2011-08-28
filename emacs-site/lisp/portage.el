;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; ebuild/portage editing since I use Emacs, not Vi

(define-derived-mode ebuild-mode sh-mode "Ebuild"
  "Major mode for editing Gentoo Portage files."
  (make-local-variable 'text-mode-variant)
  (sh-set-shell "bash")
  (setq text-mode-variant t)
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (make-local-variable 'tab-width)
  (setq tab-width 4)
  )

(provide 'portage)
