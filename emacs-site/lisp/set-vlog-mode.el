;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'vlog-mode)

;; settings for vlog mode
(setq vlog-mode-highlight-all-uppercase-words t)
(setq vlog-align-mod-inst-stop-list '(28 52))
(setq vlog-indent-level-beh                2
      vlog-indent-level-block              0
      vlog-indent-level-block-beh          0
      vlog-indent-level-block-inside       2
      vlog-indent-level-case-inside        4
      vlog-indent-level-case-branch-inside 2
      vlog-indent-level-cond               2
      vlog-indent-level-default            2
      vlog-indent-level-port-list          4)
(setq vlog-skel-user-name    "Your name"
      vlog-skel-company-name "GNU")

(vlog-mode-enable-v2k)
(vlog-mode-make-keywords)
(setq vlog-mode-keywordset-docs
      (append vlog-mode-keywordset-docs
              (list "Note:" "NOTE:" "note:")))
                               

(provide 'set-vlog-mode)
