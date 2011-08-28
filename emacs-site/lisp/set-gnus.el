;; 
;; 

;; Authored by effecting2 in 2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

(require 'gnus)

(setq gnus-select-method '(nntp "news.gmane.org"))

(setq gnus-inhibit-startup-message t)
(setq gnus-play-startup-jingle nil)
(setq gnus-interactive-exit nil)
(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))
(setq gnus-carpal nil)
(setq gnus-treat-x-pgp-sig 'head)

; Automcatically sign when sending mails
(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
    
; who would want this anyway?
(setq gnus-use-full-window nil)
(setq gnus-window-min-height 10)
(setq gnus-window-min-width 70)

(setq message-mail-user-agent nil)

(provide 'set-gnus)
