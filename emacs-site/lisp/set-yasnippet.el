;; 
;; 

;; Authored by effecting2 in 2010.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'yasnippet)

(yas/initialize)
(when (not (boundp 'snippets-dir))
  (setq snippets-dir "~/.emacs.d/snippets"))
(yas/load-directory snippets-dir)

(provide 'set-yasnippet)
