;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(provide 'set-elscreen)

(setq elscreen-display-tab t)
(setq elscreen-mode-to-nickname-alist
      (quote (("^dired-mode$" lambda nil (format "Dired(%s)" dired-directory))
              ("^customize-mode$" lambda nil (format "Cust(%s)" (buffer-name (current-buffer))))
              ("^Info-mode$" lambda nil (format "Info(%s)" (file-name-nondirectory Info-current-file)))
              ("^mh-folder-mode$" lambda nil (format "MH-E(%s)" (buffer-name (current-buffer))))
              ("^mh-letter-mode" lambda nil (format "MH-E(%s)" (buffer-name (current-buffer))))
              ("^irchat-" . "IRChat") ("^liece-" . "Liece") ("^lookup-" . "Lookup"))
             ))
(setq elscreen-startup-command-line-processing nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display-kill-screen nil)

(require 'elscreen)
