;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'footnote)

(setq footnote-section-tag "links: ")
(setq footnote-section-tag-regexp "links\\(\\[.\\]\\)?: ")
(setq footnote-spaced-footnotes nil)
(setq footnote-narrow-to-footnotes-when-editing nil)

(provide 'set-footnote)
