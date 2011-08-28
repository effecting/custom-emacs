;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'speedbar)

;; Configure the Speedbar to more useful settings.

;; no long wrapping directories
(setq-default speedbar-directory-button-trim-method 'trim)

;; ignore a wider variety of version control directories
;; and also ignore all hidden directories
(setq-default speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.bzr\\|\\.git\\|\\.svn\\|\\.[A-Za-z0-9._-]\\{1,\\}\\)\\'")

;; support a wider variety of file extensions
;; especially for SGML/XML/HTML languages and HDL languages
(setq-default speedbar-supported-extension-expressions (quote (".\\(xml\\|xslt?\\)" ".mk" ".v" ".vhd.?" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?" ".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".\\(s\\|x\\|p\\)?html?" "[Mm]akefile\\(\\.in\\)?")))

(provide 'set-speedbar)
