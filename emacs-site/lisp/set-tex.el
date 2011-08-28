;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'tex-mode)

;; loading of auctex
(load "auctex.el" nil t t)

(require 'tex)
;; setting for LaTeX mode with Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(require 'latex)
;; Auctex support for `listings' package
(add-to-list 'LaTeX-verbatim-environments "lstlisting")
(add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

(require 'bib-cite)
;; Auctex bibcite support for reftex
(setq bib-cite-use-reftex-view-crossref t)

(provide 'set-tex)
