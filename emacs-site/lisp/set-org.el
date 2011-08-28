;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(provide 'set-org)

(require 'cl) ; a fix for bad deps in org.el 4.72
(require 'org)

(setq org-agenda-include-diary t)

(defadvice org-mode (after my-org-mode activate)
  "org-mode overrides many of the useful outline-mode keys,
such as hiding nodes, etc.; so pick a few that are similar
keys to their `show-' counterparts."
  (local-set-key (kbd "C-c a") 'org-agenda)
  (local-set-key (kbd "C-c l") 'org-store-link)
  (local-set-key (kbd "C-c M-a") 'hide-sublevels)
  (local-set-key (kbd "C-c M-k") 'hide-subtree)
  (local-set-key (kbd "C-c M-b") 'hide-body)
  (local-set-key (kbd "C-c v") 'show-subtree)
  )

(setq org-log-done 'time)

